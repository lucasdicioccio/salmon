{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

{- | How each graph operation scales, per shape: every operation below run
over the "Test.GraphFixture" shapes at growing sizes, timed, with the bytes
it allocated, each series stopped once a step passes its budget.

Not part of @cabal test@ and not built by @cabal build all@ (it is a
@benchmark@ stanza): it is slow, and the numbers only mean something on a
quiet machine.

@
cabal bench salmon-ops-recipes:graph-profiles \\
  --benchmark-options='--max-size 100000 --heap 4g --out profiles.tsv'
cabal bench salmon-ops-recipes:graph-profiles \\
  --benchmark-options='--summarise profiles.tsv'
@

Each series (one operation over one shape) runs in a process of its own,
see 'measureAll' for why.

= What a row is

One operation, one shape, one size: the wall-clock time of the operation
alone (inputs are built and forced before the clock starts, a major
collection runs first) and the bytes allocated meanwhile, on every thread.
A step that takes under a quarter of a second is run three times and the
fastest kept.

= Two size axes

An operation that reads the expanded graph ('Salmon.Op.Dag.foldDag', and so
a declaration in @serve@) pays per __occurrence__: per path from the root
to a node. On a shape that shares nodes that number is exponential in the
node count, so those series are sized by occurrences and reach a few dozen
nodes. An operation that reads a 'Dag' pays per __node__, and is given one
built by 'Test.GraphFixture.dagOf' without going through the fold, at
whatever node count the cap allows.

= When a series stops

At the size cap; when a step took more than a third of the budget (the next
one is three times larger, so it would be over); when a step was still
running at three times the budget (it is abandoned); when building its
inputs took two budgets; or when the heap bound (@--heap@) was hit. Which of
these ended a series is recorded.
-}
module Main (main) where

import Control.Comonad.Cofree (Cofree (..))
import Control.Concurrent.STM (atomically, readTVar, retry)
import Control.Exception (AsyncException (HeapOverflow), Exception, bracket, evaluate, finally, throwIO, try)
import Control.Monad (forM, forM_, unless, when)
import Control.Monad.Identity (runIdentity)
import Data.Aeson (Value (..), encode, toJSON)
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as LByteString
import Data.Dynamic (toDyn)
import Data.Foldable (toList)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.List (foldl', intercalate, nub)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isNothing, mapMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock (getCurrentTime)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Stats (allocated_bytes, getRTSStats, getRTSStatsEnabled, max_mem_in_use_bytes)
import Options.Applicative hiding (help)
import qualified Options.Applicative as O
import System.Environment (getExecutablePath)
import System.Exit (ExitCode (..), exitFailure)
import System.IO (IOMode (AppendMode, WriteMode), hClose, hFlush, hPutStr, hPutStrLn, openFile, stderr, stdout)
import System.Mem (performMajorGC, performMinorGC)
import System.Posix.Process (exitImmediately)
import System.Process (CreateProcess (..), StdStream (NoStream), createPipe, createProcess, proc, terminateProcess, waitForProcess)
import System.Timeout (timeout)
import Text.Printf (printf)
import Text.Read (readMaybe)

import GHC.IO.Handle (hDuplicate, hDuplicateTo)

import qualified Salmon.Actions.Concurrent as Concurrent
import qualified Salmon.Actions.Dot as Dot
import qualified Salmon.Actions.Help as Help
import qualified Salmon.Actions.Query as Query
import Salmon.Actions.Serve (Convergence (..), NodeState (..))
import qualified Salmon.Actions.Serve as Serve
import qualified Salmon.Actions.Serve.Http as Http
import qualified Salmon.Actions.Serve.StatusSink as StatusSink
import qualified Salmon.Actions.UpDown as UpDown
import qualified Salmon.Actions.Upkeep as Upkeep
import Salmon.Builtin.Extension (Extension (..), Op, evalDeps, nodeps, op, opAct)
import qualified Salmon.Client.Model as Model
import Salmon.Op.Actions (Act (..))
import Salmon.Op.Configure (Configure (..))
import Salmon.Op.Dag (Dag (..))
import qualified Salmon.Op.Dag as Dag
import qualified Salmon.Op.Ledger as Ledger
import Salmon.Op.Ref (Ref, mkRef)
import qualified Salmon.Op.Rewrite as Rewrite
import Salmon.Op.Status (Direction (..), Stability (..), Status (..))
import Salmon.Op.Track (Track (..))
import Salmon.Reporter (ReporterM (..), silent)
import Salmon.Reporter.Tagged (Tagged (..))

import Test.GraphFixture

-------------------------------------------------------------------------------
-- options

data Options' = Options'
    { optMaxSize :: Int
    , optMaxOccurrences :: Integer
    , optBudget :: Double
    , optOut :: Maybe FilePath
    , optOnly :: [String]
    , optShapes :: [String]
    , optSummarise :: Maybe FilePath
    , optList :: Bool
    , optHeap :: String
    , optSeries :: Maybe String
    }

optionsP :: Parser Options'
optionsP =
    Options'
        <$> option auto (long "max-size" <> metavar "NODES" <> value 10000 <> showDefault <> O.help "The size cap: no graph has more nodes than this.")
        <*> option auto (long "max-occurrences" <> metavar "N" <> value 30000000 <> showDefault <> O.help "The cap for operations that read the expanded graph: no expansion has more nodes than this.")
        <*> option auto (long "budget" <> metavar "SECONDS" <> value 10 <> showDefault <> O.help "A series stops after a step over a third of this, and a step is abandoned at three times this.")
        <*> optional (strOption (long "out" <> metavar "FILE" <> O.help "Append the rows to this file (tab-separated). Standard output otherwise."))
        <*> many (strOption (long "only" <> metavar "TEXT" <> O.help "Only the operations whose group or name contains this. Repeatable."))
        <*> many (strOption (long "shape" <> metavar "NAME" <> O.help "Only this shape (see --list). Repeatable."))
        <*> optional (strOption (long "summarise" <> metavar "FILE" <> O.help "Measure nothing: print the table of the rows in this file, as markdown."))
        <*> switch (long "list" <> O.help "Measure nothing: list the operations and the shapes.")
        <*> strOption (long "heap" <> metavar "SIZE" <> value "4g" <> showDefault <> O.help "The heap bound of each series (its process runs with +RTS -M<SIZE>).")
        <*> optional (strOption (long "series" <> metavar "SHAPE:INDEX" <> internal))

main :: IO ()
main = do
    opts <- execParser (info (optionsP <**> helper) (fullDesc <> progDesc "Time the graph operations over synthetic graphs of growing size."))
    case (optList opts, optSummarise opts, optSeries opts) of
        (True, _, _) -> do
            forM_ shapes $ \(name, shape) -> putStrLn ("shape\t" <> name <> "\t" <> show shape)
            forM_ benches $ \b -> putStrLn ("operation\t" <> benchGroup b <> "\t" <> benchName b <> "\t" <> scaleName (benchScale b))
        (_, Just path, _) -> summarise path
        (_, _, Just series)
            | (shapeName, ':' : index) <- break (== ':') series
            , Just shape <- lookup shapeName shapes
            , Just i <- readMaybe index
            , b : _ <- drop i benches ->
                measureSeries opts shapeName shape b
            | otherwise -> hPutStrLn stderr ("no such series: " <> series) >> exitFailure
        _ -> measureAll opts

-------------------------------------------------------------------------------
-- shapes and sizes

shapes :: [(String, Shape)]
shapes =
    [ ("chain", Chain)
    , ("fan", Fan)
    , ("tree2", Tree 2)
    , ("diamonds2", Diamonds 2)
    , ("layered8x2", Layered 8 2)
    , ("random8x3", RandomLayered 8 3)
    , ("dense32", Layered 32 32)
    ]

seed :: Seed
seed = 3

-- | What a series is sized by, and fitted against.
data Scale = PerNode | PerOccurrence
    deriving (Eq)

scaleName :: Scale -> String
scaleName PerNode = "nodes"
scaleName PerOccurrence = "occurrences"

-- | 100, 300, 1000, 3000, ...
targets :: [Integer]
targets = concat [[10 ^ k, 3 * 10 ^ k] | k <- [2 :: Int ..]]

-- | The requested sizes a series visits: for each target, the largest size
-- under the cap whose node count (or occurrence count) does not exceed it.
sizesFor :: Options' -> Scale -> Shape -> [Int]
sizesFor opts scale shape = dedupe (mapMaybe fit (takeWhile (<= ceiling') targets))
  where
    ceiling' = case scale of
        PerNode -> fromIntegral (optMaxSize opts)
        PerOccurrence -> optMaxOccurrences opts
    metric n = case scale of
        PerNode -> (counts shape n).countNodes
        PerOccurrence -> (counts shape n).countOccurrences
    fit t =
        let hi = fromIntegral (min t (fromIntegral (optMaxSize opts)))
            n = search 1 hi
         in if metric n <= t then Just n else Nothing
      where
        -- the largest n in [lo, hi] with metric n <= t; the metric never
        -- decreases with the requested size.
        search lo hi
            | lo >= hi = lo
            | otherwise =
                let mid = (lo + hi + 1) `div` 2
                 in if metric mid <= t then search mid hi else search lo (mid - 1)
    -- two requests that realise the same graph are one step.
    dedupe = go Nothing
      where
        go _ [] = []
        go prev (n : rest)
            | Just (counts shape n).countNodes == prev = go prev rest
            | otherwise = n : go (Just (counts shape n).countNodes) rest

-------------------------------------------------------------------------------
-- measuring

data Sample = Sample
    { sampleLabel :: String
    , sampleSecs :: Double
    , sampleAlloc :: Word64
    }

allocated :: IO Word64
allocated = allocated_bytes <$> getRTSStats

-- | One run of an action whose result is forced by the 'Int' it returns.
timed :: IO Int -> IO (Double, Word64)
timed act = do
    performMajorGC
    a0 <- allocated
    t0 <- getMonotonicTimeNSec
    n <- act
    _ <- evaluate n
    t1 <- getMonotonicTimeNSec
    -- the counter is only brought up to date by a collection.
    performMinorGC
    a1 <- allocated
    pure (fromIntegral (t1 - t0) / 1e9, a1 - a0)

{- | The outer action prepares one run and returns what to time, so that a
repeat never finds a structure the previous run already evaluated.
-}
best :: IO (IO Int) -> IO (Double, Word64)
best prepare = do
    first <- prepare >>= timed
    if fst first >= 0.25
        then pure first
        else do
            more <- forM [1 :: Int, 2] (const (prepare >>= timed))
            pure (foldl (\a b -> if fst b < fst a then b else a) first more)

-- | Time a sequence of steps that hand state on to each other, once each.
phases :: [(String, IO Int)] -> IO [Sample]
phases steps = forM steps $ \(label, act) -> do
    (secs, bytes) <- timed act
    pure (Sample label secs bytes)

data Env = Env
    { envShape :: Shape
    , envSize :: Int
    , envBudget :: Double
    , envFresh :: IO Op
    -- ^ a new root each time: an 'Op' keeps the predecessors it has been
    -- asked for, so a root that was expanded once is no longer a
    -- description of a graph but the graph.
    , envFreshManaged :: IO Op
    , envDag :: Dag Extension
    -- ^ shared by every operation at this size; built by 'dagOf'
    , envPaths :: Map.Map Ref [Text]
    -- ^ what @serve@ lists per node, see 'pathsOf'
    }

data Bench = Bench
    { benchGroup :: String
    , benchName :: String
    , benchScale :: Scale
    , benchRun :: Env -> IO [Sample]
    }

-- | An operation with one number to report.
single :: String -> String -> Scale -> (Env -> IO (IO Int)) -> Bench
single grp name scale prepare =
    Bench grp name scale $ \e -> do
        (secs, bytes) <- best (prepare e)
        pure [Sample "" secs bytes]

-- | An operation over the shared 'Dag', forced before the clock starts.
overDag :: String -> String -> (Dag Extension -> IO (IO Int)) -> Bench
overDag grp name prepare =
    single grp name PerNode $ \e -> do
        _ <- evaluate (weighDag e.envDag)
        prepare e.envDag

-- | Building an operation's inputs took too long: the series ends, and the
-- operation itself was not what ended it.
data InputsOver = InputsOver
    deriving (Show)

instance Exception InputsOver

row :: Bench -> String -> Shape -> Int -> String -> Double -> Word64 -> String -> String
row b shapeName shape size label secs bytes status =
    intercalate
        "\t"
        [ benchGroup b
        , benchName b
        , label
        , scaleName (benchScale b)
        , shapeName
        , show size
        , show c.countNodes
        , show c.countEdges
        , abbreviated c.countOccurrences
        , printf "%.6f" secs
        , show bytes
        , status
        ]
  where
    c = counts shape size
    -- a shape that shares has more occurrences than digits worth writing:
    -- past eighteen of them only the magnitude is kept, as @1e<exponent>@.
    abbreviated n =
        let digits = show n
         in if length digits <= 18 then digits else "1e" <> show (length digits - 1)

{- | Every series in a process of its own, one after the other.

A step that is abandoned leaves its threads behind (a supervisor's machines,
a loop mid-pass), and they keep waking: measured in one process, every
series after the first abandoned step came out hundreds of times slower
than it is. A series ends at its first abandoned step, so in a process of
its own nothing is measured after one.
-}
measureAll :: Options' -> IO ()
measureAll opts = do
    self <- getExecutablePath
    let selected = [(i, b) | (i, b) <- zip [0 :: Int ..] benches, null (optOnly opts) || any (\o -> isIn o (benchGroup b) || isIn o (benchName b)) (optOnly opts)]
        chosen = [s | s@(name, _) <- shapes, null (optShapes opts) || name `elem` optShapes opts]
    forM_ chosen $ \(shapeName, shape) ->
        forM_ selected $ \(i, b) -> do
            let args =
                    ["--max-size", show (optMaxSize opts), "--max-occurrences", show (optMaxOccurrences opts), "--budget", show (optBudget opts)]
                        <> concat [["--out", path] | Just path <- [optOut opts]]
                        <> ["--series", shapeName <> ":" <> show i, "+RTS", "-M" <> optHeap opts, "-RTS"]
            hFlush stdout
            (_, _, _, child) <- createProcess (proc self args){std_in = NoStream}
            -- a series is a dozen steps of at most three budgets each.
            ended <- timeout (round (60 * optBudget opts * 1e6)) (waitForProcess child)
            died <- case ended of
                Just ExitSuccess -> pure Nothing
                Just (ExitFailure code) -> pure (Just ("stop:exit " <> show code))
                Nothing -> do
                    terminateProcess child
                    _ <- waitForProcess child
                    pure (Just "stop:killed")
            forM_ died $ \why -> do
                let line = row b shapeName shape 0 "" 0 0 why
                maybe (putStrLn line) (`appendFile` (line <> "\n")) (optOut opts)
  where
    isIn needle hay = Text.pack needle `Text.isInfixOf` Text.pack hay

-- | One operation over one shape, at growing sizes, until it stops.
measureSeries :: Options' -> String -> Shape -> Bench -> IO ()
measureSeries opts shapeName shape b = do
    enabled <- getRTSStatsEnabled
    unless enabled $ do
        hPutStrLn stderr "the allocation counter is off: run with +RTS -T"
        exitFailure
    out <- maybe (pure stdout) (`openFile` AppendMode) (optOut opts)
    let scheduled = sizesFor opts (benchScale b) shape
        steps size = do
            let c = counts shape size
                say label secs bytes status = hPutStrLn out (row b shapeName shape size label secs bytes status) >> hFlush out
            hPutStrLn stderr (shapeName <> " " <> show c.countNodes <> " nodes: " <> benchGroup b <> " / " <> benchName b)
            env <- newEnv (optBudget opts) shape size
            outcome <- try (try (timeout (round (3 * optBudget opts * 1e6)) (benchRun b env)))
            case outcome of
                Left HeapOverflow -> say "" 0 0 "stop:heap" >> pure False
                Left other -> throwIO other
                Right (Left InputsOver) -> say "" 0 0 "stop:inputs" >> pure False
                Right (Right Nothing) -> say "" 0 0 "stop:timeout" >> pure False
                Right (Right (Just samples)) -> do
                    forM_ samples $ \s -> say (sampleLabel s) (sampleSecs s) (sampleAlloc s) "ok"
                    if maximum (0 : fmap sampleSecs samples) > optBudget opts / 3
                        then say "" 0 0 "stop:budget" >> pure False
                        else
                            if size == last scheduled
                                then say "" 0 0 "stop:cap" >> pure False
                                else pure True
        go [] = pure ()
        go (size : rest) = do
            more <- steps size
            performMajorGC
            when more (go rest)
    go scheduled
    peak <- max_mem_in_use_bytes <$> getRTSStats
    hPutStrLn stderr ("peak memory in use: " <> show (peak `div` (1024 * 1024)) <> " MiB")
    hClose out
    -- not a normal return: whatever an abandoned step left running is not waited for.
    hFlush stdout
    exitImmediately ExitSuccess

newEnv :: Double -> Shape -> Int -> IO Env
newEnv budget shape size = do
    -- read back through a reference so that no two calls can be shared.
    cell <- newIORef seed
    let fresh options = do
            s <- readIORef cell
            pure (generateWith options shape size s)
    root <- fresh fixtureOptions
    pathRoot <- fresh fixtureOptions
    pure
        Env
            { envShape = shape
            , envSize = size
            , envBudget = budget
            , envFresh = fresh fixtureOptions
            , envFreshManaged = fresh fixtureOptions{optManaged = True}
            , envDag = dagOf root
            , envPaths = pathsOf pathRoot
            }

-- | A marker one node in ten carries, sixteen values of it: what the
-- rewrites below collect.
newtype Batch = Batch Int
    deriving (Show, Eq, Ord)

{- | The nodes every series runs over: no-ops whose @check@ answers
'UpDown.Immaterial' (what a node with no check says, so an @up@ pass runs
every @up@), one in ten carrying a 'Batch'.
-}
fixtureOptions :: Options
fixtureOptions =
    defaultOptions
        { optDynamics = \i -> [toDyn (Batch ((i `div` 10) `mod` 16)) | i `mod` 10 == 0]
        }

-------------------------------------------------------------------------------
-- forcing

weighDag :: Dag ext -> Int
weighDag dag =
    Map.size (dagNodes dag)
        + sum (fmap length (Map.elems (dagDependencies dag)))
        + sum (fmap length (Map.elems (dagDependants dag)))
        + length (dagOrderRev dag)
        + length (dagConflicts dag)

weighTexts :: [Text] -> Int
weighTexts = foldl' (\n t -> n + Text.length t) 0

weighPaths :: Map.Map Ref [Text] -> Int
weighPaths = Map.foldl' (\n ts -> n + weighTexts ts) 0

weighBytes :: LByteString.ByteString -> Int
weighBytes = fromIntegral . LByteString.length

-- | The number of nodes of an expansion, without the stack a recursion
-- over a deep one would take.
weighCofree :: Cofree f a -> ([Cofree f a] -> f (Cofree f a) -> [Cofree f a]) -> Int
weighCofree root push = go 0 [root]
  where
    go !n [] = n
    go !n ((_ :< g) : rest) = go (n + 1) (push rest g)

expansionSize :: Op -> Int
expansionSize root = weighCofree (evalDeps root) (\rest g -> toList g <> rest)

weighModel :: Model.Model -> Int
weighModel m =
    length m.modelOrder
        + Map.foldl' (\n x -> n + Text.length x.nodeShorthand + length x.nodeDependencies + length x.nodeDependants + length x.nodePaths + Text.length x.nodeConvergence) 0 m.modelNodes

-- | Run an action with standard output going nowhere: two of the renders
-- print rather than return.
quietly :: IO a -> IO a
quietly act =
    bracket
        ( do
            hFlush stdout
            saved <- hDuplicate stdout
            devNull <- openFile "/dev/null" WriteMode
            hDuplicateTo devNull stdout
            hClose devNull
            pure saved
        )
        (\saved -> hFlush stdout >> hDuplicateTo saved stdout >> hClose saved)
        (const act)

-------------------------------------------------------------------------------
-- the operations

{- | The paths @serve@ lists for each node ('Serve.worldPaths', for a world
with one declaration): at most 'Query.pathLimit' per node.
-}
pathsOf :: Op -> Map.Map Ref [Text]
pathsOf root =
    Map.map (fmap (Text.intercalate "/")) (Query.outlinePaths Query.pathLimit (Query.outline (evalDeps root)))

benches :: [Bench]
benches =
    concat
        [ foldBenches
        , dagBenches
        , walkBenches
        , upkeepBenches
        , rewriteBenches
        , [ledgerBench]
        , queryBenches
        , [serveBench]
        , renderBenches
        , clientBenches
        ]

foldBenches :: [Bench]
foldBenches =
    [ single "fold" "expand (evalDeps, every occurrence visited)" PerOccurrence $ \e -> do
        root <- e.envFresh
        pure (evaluate (expansionSize root))
    , single "fold" "expand + foldDag" PerOccurrence $ \e -> do
        root <- e.envFresh
        pure (evaluate (weighDag (Dag.foldDag Dag.sameRepresentative (evalDeps root))))
    , single "fold" "dagOf (the fixture's per-node build, for comparison)" PerNode $ \e -> do
        root <- e.envFresh
        pure (evaluate (weighDag (dagOf root)))
    ]

dagBenches :: [Bench]
dagBenches =
    [ overDag "dag" "fromMagma" $ \dag -> do
        let magma = dagNodes dag
            edges = Dag.dagEdges dag
        _ <- evaluate (Map.size magma + Set.size edges)
        pure (evaluate (weighDag (Dag.fromMagma magma edges)))
    , overDag "dag" "mergeDag into an empty Dag" $ \dag ->
        pure (evaluate (weighDag (Dag.mergeDag Dag.sameRepresentative Dag.emptyDag dag)))
    , overDag "dag" "mergeDag onto itself" $ \dag ->
        pure (evaluate (weighDag (Dag.mergeDag Dag.sameRepresentative dag dag)))
    , overDag "dag" "dagEdges" $ \dag ->
        pure (evaluate (Set.size (Dag.dagEdges dag)))
    , overDag "dag" "roots + leaves" $ \dag ->
        pure (evaluate (length (Dag.roots dag) + length (Dag.leaves dag)))
    , overDag "dag" "stuck (waiting on dependencies)" $ \dag ->
        pure (evaluate (Set.size (Dag.stuck Dag.dependenciesOf dag)))
    , overDag "dag" "stuck (waiting on dependants)" $ \dag ->
        pure (evaluate (Set.size (Dag.stuck Dag.dependantsOf dag)))
    ]

walkBenches :: [Bench]
walkBenches =
    [ overDag "walk" "upDag (sequential)" $ \dag ->
        pure (fromEnum <$> UpDown.upDag UpDown.alwaysRequired silent dag)
    , overDag "walk" "downDag (sequential)" $ \dag ->
        pure (fromEnum <$> UpDown.downDag UpDown.alwaysRequired silent dag)
    , overDag "walk" "upDagConcurrent" $ \dag ->
        pure (fromEnum <$> Concurrent.upDagConcurrent UpDown.alwaysRequired silent Concurrent.noMailboxes Nothing dag)
    , overDag "walk" "downDagConcurrent" $ \dag ->
        pure (fromEnum <$> Concurrent.downDagConcurrent UpDown.alwaysRequired silent Concurrent.noMailboxes Nothing dag)
    ]

-- | Wait until every machine has settled.
awaitStable :: Upkeep.Supervisor Extension -> IO Int
awaitStable sup = do
    forM_ (Map.elems (Upkeep.supervisorStatuses sup)) $ \var ->
        atomically $ do
            st <- readTVar var
            unless (st.statusStability == Stable) retry
    pure (Map.size (Upkeep.supervisorStatuses sup))

-- | Stop a supervisor a step left behind, whatever it was holding.
standDown :: IORef (Maybe (Upkeep.Supervisor Extension)) -> IO ()
standDown cell = do
    left <- readIORef cell
    writeIORef cell Nothing
    forM_ left $ \sup -> do
        kept <- Upkeep.stopUpkeep sup
        _ <- Upkeep.releaseKept silent (const False) kept
        pure ()

upkeepBenches :: [Bench]
upkeepBenches =
    [ Bench "upkeep" "tend nodes already up (settled)" PerNode $ \e -> do
        _ <- evaluate (weighDag e.envDag)
        cell <- newIORef Nothing
        flip finally (standDown cell) $
            phases
                [
                    ( "startUpkeep"
                    , do
                        sup <- Upkeep.startUpkeep silent Upkeep.noKept (const (Just (Upkeep.Tend TurnUp Upkeep.Settled))) e.envDag
                        writeIORef cell (Just sup)
                        pure (Map.size (Upkeep.supervisorStatuses sup))
                    )
                , ("until every machine is stable", readIORef cell >>= maybe (pure 0) awaitStable)
                , ("stopUpkeep", standDown cell >> pure 0)
                ]
    , Bench "upkeep" "bring nodes up (unsettled)" PerNode $ \e -> do
        _ <- evaluate (weighDag e.envDag)
        cell <- newIORef Nothing
        flip finally (standDown cell) $
            phases
                [
                    ( "startUpkeep"
                    , do
                        sup <- Upkeep.startUpkeep silent Upkeep.noKept (const (Just (Upkeep.Tend TurnUp Upkeep.Unsettled))) e.envDag
                        writeIORef cell (Just sup)
                        pure (Map.size (Upkeep.supervisorStatuses sup))
                    )
                , ("until every machine is stable", readIORef cell >>= maybe (pure 0) awaitStable)
                , ("stopUpkeep", standDown cell >> pure 0)
                ]
    , Bench "upkeep" "managed nodes: hold, hand over, adopt" PerNode $ \e -> do
        root <- e.envFreshManaged
        let dag = dagOf root
            tend = const (Just (Upkeep.Tend TurnUp Upkeep.Unsettled))
        _ <- evaluate (weighDag dag)
        cell <- newIORef Nothing
        kept <- newIORef Upkeep.noKept
        let release = do
                standDown cell
                held <- readIORef kept
                writeIORef kept Upkeep.noKept
                _ <- Upkeep.releaseKept silent (const False) held
                pure ()
        flip finally release $
            phases
                [
                    ( "startUpkeep"
                    , do
                        sup <- Upkeep.startUpkeep silent Upkeep.noKept tend dag
                        writeIORef cell (Just sup)
                        pure (Map.size (Upkeep.supervisorStatuses sup))
                    )
                , ("until every machine is stable", readIORef cell >>= maybe (pure 0) awaitStable)
                ,
                    ( "stopUpkeep (machines kept)"
                    , do
                        left <- readIORef cell
                        writeIORef cell Nothing
                        forM_ left $ \sup -> Upkeep.stopUpkeep sup >>= writeIORef kept
                        Map.size . Upkeep.keptHeld <$> readIORef kept
                    )
                ,
                    ( "startUpkeep (adopting)"
                    , do
                        held <- readIORef kept
                        writeIORef kept Upkeep.noKept
                        sup <- Upkeep.startUpkeep silent held tend dag
                        writeIORef cell (Just sup)
                        pure (Map.size (Upkeep.supervisorStatuses sup))
                    )
                , ("stopUpkeep + releaseKept", release >> pure 0)
                ]
    ]

rewriteBenches :: [Bench]
rewriteBenches =
    [ overDag "rewrite" "no phase registered (wholeGraph + rewrite)" $ \dag ->
        pure (evaluate (weighRewritten (Rewrite.rewrite [] (Rewrite.wholeGraph dag) dag)))
    , overDag "rewrite" "one batch of a tenth of the nodes" $ \dag ->
        pure (evaluate (weighRewritten (Rewrite.rewrite [oneBatch] (Rewrite.wholeGraph dag) dag)))
    , overDag "rewrite" "sixteen batches of a tenth of the nodes" $ \dag ->
        pure (evaluate (weighRewritten (Rewrite.rewrite [sixteenBatches] (Rewrite.wholeGraph dag) dag)))
    ]
  where
    weighRewritten r = weighDag (Rewrite.computedDag r) + sum (fmap Set.size (Map.elems (Rewrite.computedMembers r)))

    oneBatch :: Rewrite.Rewrite Extension
    oneBatch _ r =
        Rewrite.introduce (batchAct 0) (Set.fromList [aref | (aref, _ :: [Batch]) <- Rewrite.collectDynamic r]) r

    sixteenBatches :: Rewrite.Rewrite Extension
    sixteenBatches _ r =
        let groups = Map.fromListWith Set.union [(k, Set.singleton aref) | (aref, Batch k : _) <- Rewrite.collectDynamic r]
         in foldl' (\acc (k, members) -> Rewrite.introduce (batchAct (k + 1)) members acc) r (Map.toList groups)

    batchAct :: Int -> Act Extension
    batchAct k =
        fromMaybe (error "a batch node with no action") $
            opAct (op "batch" nodeps (\x -> x{ref = mkRef "graph-profiles-batch" k, help = "a batch"}))

{- | Eight declarations over the same graph, each leaving out a different
eighth of its nodes: several declarations that mostly overlap.
-}
ledgerBench :: Bench
ledgerBench =
    Bench "ledger" "eight overlapping declarations" PerNode $ \e -> do
        let dag = e.envDag
            position = Map.fromList (zip (Dag.dagOrder dag) [0 :: Int ..])
            whole = Ledger.contribution dag
            keeps k r = maybe False (\i -> i `mod` 8 /= k) (Map.lookup r position)
            part k =
                whole
                    { Ledger.contribRefs = Set.filter (keeps k) (Ledger.contribRefs whole)
                    , Ledger.contribEdges = Set.filter (\(a, b) -> keeps k a && keeps k b) (Ledger.contribEdges whole)
                    }
            ledger = foldl' (\l k -> Ledger.declare k (part k) l) Ledger.emptyLedger [0 .. 7 :: Int]
        _ <- evaluate (weighDag dag)
        _ <- evaluate (sum [Set.size (Ledger.contribRefs c) + Set.size (Ledger.contribEdges c) | c <- Map.elems ledger])
        phases
            [ ("contribution (of one Dag)", evaluate (let c = Ledger.contribution dag in Set.size (Ledger.contribRefs c) + Set.size (Ledger.contribEdges c)))
            , ("desired", evaluate (Set.size (Ledger.desired ledger)))
            , ("precedenceOf", evaluate (Set.size (Ledger.precedenceOf ledger)))
            , ("knownRefs", evaluate (Set.size (Ledger.knownRefs ledger)))
            , ("retractAll + collect, nothing standing", evaluate (Map.size (Ledger.collect (const False) (Ledger.retractAll ledger))))
            ]

queryBenches :: [Bench]
queryBenches =
    [ single "query" "outline" PerNode $ \e -> do
        root <- e.envFresh
        pure (evaluate (Set.size (Query.outlineRefs (Query.outline (evalDeps root)))))
    , single "query" "paths per node (outline + outlinePaths, as worldPaths)" PerNode $ \e -> do
        root <- e.envFresh
        pure (evaluate (weighPaths (pathsOf root)))
    , single "query" "resolveSelectors (select **, exclude **/n1/**)" PerNode $ \e -> do
        root <- e.envFresh
        pure $ do
            let (selected, excluded) = Query.resolveSelectors (evalDeps root) ["**"] ["**/n1/**"]
            evaluate (Set.size selected + Set.size excluded)
    , single "query" "query show --dedupe (renderAnnotated)" PerNode $ \e -> do
        root <- e.envFresh
        pure (evaluate (weighTexts (Query.renderAnnotated (evalDeps root) Set.empty Set.empty True True)))
    , single "query" "query show, every path (renderAnnotated)" PerOccurrence $ \e -> do
        root <- e.envFresh
        pure (evaluate (weighTexts (Query.renderAnnotated (evalDeps root) Set.empty Set.empty False True)))
    , single "query" "query plan (expand, fold, resolve, encode)" PerOccurrence $ \e -> do
        root <- e.envFresh
        pure $ do
            -- what the command line does for @query plan@, minus reading the directive.
            let cograph = evalDeps root
            dag <- UpDown.expandDag silent (pure . runIdentity) root
            let computed = Rewrite.rewrite [] (Rewrite.wholeGraph dag) dag
                (_, excluded) = Query.resolveRewrittenSelectors cograph computed [] ["**/n1/**"]
                plan = Query.Plan "digest" (Set.toList excluded) ["**/n1/**"] Nothing
            evaluate (weighBytes (encode plan))
    , overDag "query" "run tree (dagLines)" $ \dag ->
        pure (evaluate (weighTexts (Help.dagLines dag)))
    , overDag "query" "run dag (printDagCograph, to /dev/null)" $ \dag ->
        pure (quietly (Dot.printDagCograph dag) >> pure 0)
    ]

{- | The loop itself, over a scripted input: supervision off (so nothing
tends between commands), one declaration, @status@, an idle @converge@,
@clear@. The steps are the intervals between the loop's own reports, so
each is what the operator waits for; evaluation is lazy, so a step can be
charged work that an earlier one set up.
-}
serveBench :: Bench
serveBench =
    Bench "serve" "loop" PerOccurrence $ \e -> do
        marks <- newIORef []
        let mark name = do
                t <- getMonotonicTimeNSec
                performMinorGC
                a <- allocated
                t' <- getMonotonicTimeNSec
                modifyIORef' marks ((name, t, a, t') :)
            reporter = ReporterM $ \rep -> case rep of
                Serve.Supervised _ -> mark "supervised"
                Serve.Declared{} -> mark "declared"
                Serve.ConvergeStart{} -> mark "converge-start"
                Serve.ConvergeStop{} -> mark "converge-stop"
                Serve.StatusReport _ nodes paths -> do
                    _ <- evaluate (length nodes + weighPaths paths)
                    mark "status"
                Serve.Cleared _ -> mark "cleared"
                _ -> pure ()
            parse args = case args of
                [n] | Just size <- readMaybe n -> Right (size :: Int)
                _ -> Left "expected a size"
            program = Track (\size -> generateWith fixtureOptions e.envShape size seed)
        (readEnd, writeEnd) <- createPipe
        hPutStr writeEnd (unlines ["supervise off", "up " <> show e.envSize, "status", "converge", "clear"])
        hClose writeEnd
        performMajorGC
        _ <- Serve.serveWith [] Nothing True reporter silent parse (Configure pure) program readEnd
        seen <- reverse <$> readIORef marks
        let intervals = [(from <> " -> " <> to, fromIntegral (t1 - t0) / 1e9, a1 - a0) | ((from, _, a0, t0), (to, t1, a1, _)) <- zip seen (drop 1 seen)]
            names =
                [ "record a declaration (expand, fold, ledger)"
                , "before the first pass (rewrite, worldDag)"
                , "first pass, every node up"
                , "status"
                , "before the idle pass"
                , "idle pass, nothing pending"
                , "clear"
                , "before the teardown pass"
                , "teardown pass, every node down"
                ]
            expected = ["supervised", "declared", "converge-start", "converge-stop", "status", "converge-start", "converge-stop", "cleared", "converge-start", "converge-stop"]
        if [name | (name, _, _, _) <- seen] == expected
            then pure [Sample name secs bytes | (name, (_, secs, bytes)) <- zip names intervals]
            else pure [Sample ("unexpected: " <> name) secs bytes | (name, secs, bytes) <- intervals]

-- | What the HTTP reads are answered from, for a world whose nodes are all up.
viewOf :: Env -> Http.WorldView
viewOf e =
    Http.WorldView
        { Http.viewDag = e.envDag
        , Http.viewConflicts = Map.empty
        , Http.viewNodes = nodeStates e
        , Http.viewPaths = e.envPaths
        , Http.viewHistory = []
        , Http.viewElided = 0
        }

nodeStates :: Env -> Map.Map Ref NodeState
nodeStates e =
    Map.map
        (\act -> NodeState{nodeShorthand = act.shorthand, nodeHelp = act.extension.help, nodeDirection = Serve.TurnUp, nodeConvergence = Converged, nodeStatus = Nothing})
        (dagNodes e.envDag)

{- | The inputs of a render, forced. The listed paths are the expensive one
(the query group times them on their own), so they are given two budgets
and the series ends as 'InputsOver' rather than as a slow render.
-}
readied :: Env -> IO ()
readied e = do
    _ <- evaluate (weighDag e.envDag)
    listed <- timeout (round (2 * e.envBudget * 1e6)) (evaluate (weighPaths e.envPaths))
    when (isNothing listed) (throwIO InputsOver)
    _ <- evaluate (Map.foldl' (\n st -> n + Text.length st.nodeShorthand) 0 (nodeStates e))
    pure ()

renderBenches :: [Bench]
renderBenches =
    [ single "render" "GET /dag (dagValue + encode)" PerNode $ \e -> do
        readied e
        let view = viewOf e
        pure (evaluate (weighBytes (encode (Http.dagValue Serve.Interactive view))))
    , single "render" "GET /status (status report + encode)" PerNode $ \e -> do
        readied e
        let nodes = nodeStates e
        pure (evaluate (weighBytes (encode (toJSON (FromServe (Serve.StatusReport Serve.Interactive (Map.toList nodes) e.envPaths))))))
    , single "render" "status sink document (encode)" PerNode $ \e -> do
        readied e
        let nodes = nodeStates e
        now <- getCurrentTime
        pure $
            evaluate . weighBytes . encode $
                StatusSink.Document
                    { StatusSink.docHost = "host"
                    , StatusSink.docWritten = now
                    , StatusSink.docMode = Serve.renderMode Serve.Interactive
                    , StatusSink.docLabels = []
                    , StatusSink.docStatus = toJSON (Serve.StatusReport Serve.Interactive (Map.toList nodes) e.envPaths)
                    , StatusSink.docLastConverge = Nothing
                    , StatusSink.docLastFollow = Nothing
                    }
    ]

-- | A @\/dag@ answer as a client receives it, already decoded.
dagAnswer :: Env -> IO Value
dagAnswer e = do
    readied e
    let answer = case Http.dagValue Serve.Interactive (viewOf e) of
            Object o -> Object (KeyMap.insert "seq" (Number 0) o)
            other -> other
    _ <- evaluate (weighBytes (encode answer))
    pure answer

modelOf :: Env -> IO Model.Model
modelOf e = do
    answer <- dagAnswer e
    case Model.fromDag answer of
        Left err -> error ("fromDag: " <> err)
        Right m -> do
            _ <- evaluate (weighModel m)
            pure m

doneEvents :: Model.Model -> [Model.Event]
doneEvents m = [Model.Event (Just i) "updown" "done" (Just r) Nothing Null | (i, r) <- zip [1 ..] m.modelOrder]

clientBenches :: [Bench]
clientBenches =
    [ single "client" "Model.fromDag" PerNode $ \e -> do
        answer <- dagAnswer e
        pure (evaluate (either (const 0) weighModel (Model.fromDag answer)))
    , single "client" "Model.step, one done event per node" PerNode $ \e -> do
        m <- modelOf e
        let events = doneEvents m
        _ <- evaluate (length events)
        pure (evaluate (weighModel (foldl' Model.step m events)))
    , single "client" "Model.step, a teardown: one done event per node wanted down" PerNode $ \e -> do
        m0 <- modelOf e
        let m = m0{Model.modelNodes = Map.map (\n -> n{Model.nodeDirection = "down"}) m0.modelNodes}
            events = doneEvents m
        _ <- evaluate (weighModel m + length events)
        pure (evaluate (weighModel (foldl' Model.step m events)))
    ]

-------------------------------------------------------------------------------
-- the table

data Row = Row
    { rowGroup :: String
    , rowBench :: String
    , rowLabel :: String
    , rowScale :: String
    , rowShape :: String
    , rowNodes :: Integer
    , rowOccurrences :: Integer
    , rowSecs :: Double
    , rowAlloc :: Double
    , rowStatus :: String
    }

parseRow :: String -> Maybe Row
parseRow line =
    case splitTabs line of
        [grp, bench, label, scale, shape, _, nodes, _, occurrences, secs, bytes, status] ->
            Row grp bench label scale shape <$> readMaybe nodes <*> magnitude occurrences <*> readMaybe secs <*> readMaybe bytes <*> pure status
        _ -> Nothing
  where
    magnitude s = case s of
        '1' : 'e' : k -> (10 ^) <$> (readMaybe k :: Maybe Int)
        _ -> readMaybe s
    splitTabs s = case break (== '\t') s of
        (cell, []) -> [cell]
        (cell, _ : rest) -> cell : splitTabs rest

{- | One table per group: an operation per row, a shape per column. A cell
reads @growth, largest size, time and allocation there, why it stopped@.
The growth is the slope of log time against log size over the last four
steps that took at least a millisecond.
-}
summarise :: FilePath -> IO ()
summarise path = do
    rows <- mapMaybe parseRow . lines <$> readFile path
    let groups = nub (fmap rowGroup rows)
    forM_ groups $ \grp -> do
        let inGroup = [r | r <- rows, rowGroup r == grp]
            series = nub [(rowBench r, rowLabel r) | r <- inGroup, rowStatus r == "ok"]
        putStrLn ("### " <> grp)
        putStrLn ""
        putStrLn ("| operation | sized by | " <> intercalate " | " (fmap fst shapes) <> " |")
        putStrLn ("|---|---|" <> concatMap (const "---|") shapes)
        forM_ series $ \(bench, label) -> do
            let mine = [r | r <- inGroup, rowBench r == bench]
                scale = maybe "" rowScale (safeHead mine)
                title = if null label then bench else bench <> ": " <> label
            cells <- forM shapes $ \(shapeName, _) -> do
                let points = [r | r <- mine, rowShape r == shapeName, rowLabel r == label, rowStatus r == "ok"]
                    ending = [r | r <- mine, rowShape r == shapeName, take 5 (rowStatus r) == "stop:"]
                pure (cell scale points ending)
            putStrLn ("| " <> title <> " | " <> scale <> " | " <> intercalate " | " cells <> " |")
        putStrLn ""
  where
    safeHead xs = case xs of
        [] -> Nothing
        (x : _) -> Just x

    size scale r = if scale == "occurrences" then rowOccurrences r else rowNodes r

    cell :: String -> [Row] -> [Row] -> String
    cell _ [] _ = "-"
    cell scale points ending =
        let top = last points
            why = case ending of
                [] -> "interrupted"
                (r : _) -> case drop 5 (rowStatus r) of
                    "cap" -> "cap"
                    "budget" -> "budget"
                    other
                        | rowNodes r <= 1 -> "process: " <> other
                        | otherwise -> other <> " at " <> human (size scale r)
         in growth scale points <> ", " <> human (size scale top) <> sizeNote scale top <> ": " <> seconds (rowSecs top) <> ", " <> bytes (rowAlloc top) <> " (" <> why <> ")"

    -- for a series sized by occurrences, the node count it corresponds to.
    sizeNote scale r
        | scale == "occurrences" && rowOccurrences r /= rowNodes r = " (" <> human (rowNodes r) <> " nodes)"
        | otherwise = ""

    growth :: String -> [Row] -> String
    growth scale points =
        let usable = [(log (fromIntegral (size scale r)), log (rowSecs r)) | r <- points, rowSecs r >= 0.001]
            window = drop (length usable - 4) usable
         in if length window < 3
                then "too fast to fit"
                else printf "n^%.1f" (slope window)

    slope :: [(Double, Double)] -> Double
    slope pts =
        let n = fromIntegral (length pts)
            mx = sum (fmap fst pts) / n
            my = sum (fmap snd pts) / n
            sxx = sum [(x - mx) ^ (2 :: Int) | (x, _) <- pts]
            sxy = sum [(x - mx) * (y - my) | (x, y) <- pts]
         in if sxx == 0 then 0 else sxy / sxx

    human :: Integer -> String
    human n
        | n >= 999950 = printf "%.1fM" (fromIntegral n / 1e6 :: Double)
        | n >= 1000 = printf "%.1fk" (fromIntegral n / 1e3 :: Double)
        | otherwise = show n

    seconds :: Double -> String
    seconds s
        | s >= 1 = printf "%.1f s" s
        | s >= 0.001 = printf "%.0f ms" (s * 1e3)
        | otherwise = printf "%.0f us" (s * 1e6)

    bytes :: Double -> String
    bytes b
        | b >= 1e9 = printf "%.1f GB" (b / 1e9)
        | b >= 1e6 = printf "%.0f MB" (b / 1e6)
        | otherwise = printf "%.0f kB" (b / 1e3)
