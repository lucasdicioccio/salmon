{-# LANGUAGE DeriveGeneric #-}

{- | Layer 1 coverage for "Salmon.Actions.Serve.Http" (milestone 3 of
@specs\/generic-server.md@): the @run serve@ loop with an HTTP server on a
unix socket beside its standard input, driven by a real @http-client@ over
real connections.

The nodes are the counter-bumping stubs 'Test.ServeSocketSpec' uses, plus
one whose @up@ blocks until the test lets it go. The claims: @\/dag@ is,
node for node and edge for edge, what 'Help.printDagTree' prints for the
same world; it is populated the moment something is declared, before any
pass; a retired seed's nodes stay in it wanted @down@ until they are gone;
a script typed through @POST \/command@ synchronously, asynchronously, and
on standard input leaves the same world, and the synchronous form answers
with exactly the reports each line produced; and a read answers while the
loop is inside a node's @up@. Milestone 7's static files: @GET \/@ is the
page, @\/ui\/ui.js@ is the script with its content type — one that subscribes
from a snapshot's @seq@, writes only through @POST \/command?async@, reads
@\/help\/seed@ for its seed form and never sends @quit@ — and a path outside
the embedded set is the ordinary @404@.
-}
module Test.ServeHttpSpec (tests) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (try)
import Control.Monad (forM, forM_)
import Data.Aeson (FromJSON, ToJSON, Value (..), eitherDecode, toJSON)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy.Char8 as LChar8
import Data.Foldable (toList)
import Data.List (isInfixOf, sort)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, mapMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Client.Internal (makeConnection)
import qualified Network.HTTP.Types as HTTP
import qualified Network.Socket as Socket
import qualified Network.Socket.ByteString as SocketBS
import System.FilePath ((</>))
import System.IO (Handle, IOMode (ReadMode), hClose, hPutStr, withFile)
import System.IO.Temp (withSystemTempFile)
import System.Process (createPipe)
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

import qualified Salmon.Actions.Help as Help
import qualified Salmon.Actions.Serve as Serve
import Salmon.Actions.Serve (Attributed (..), Convergence (..), Direction (..), NodeState (..), World (..))
import qualified Salmon.Actions.Serve.Http as Http
import Salmon.Builtin.Extension (Track', deps, down, help, nodeps, op, ref, up)
import Salmon.Op.Configure (Configure (..))
import Salmon.Op.Ref (Ref, mkRef)
import Salmon.Op.Track (Track (..))
import Salmon.Reporter (contramap)
import qualified Salmon.Reporter.Tagged as Tagged

import Test.Harness (capture, withTempDir)

tests :: TestTree
tests =
    testGroup
        "Salmon.Actions.Serve.Http"
        [ testCase "/dag is what printDagTree prints, node for node and edge for edge" dagMatchesPrintDagTree
        , testCase "/dag is populated from the first declaration on, before any pass" dagBeforeAnyPass
        , testCase "a retired seed's nodes stay in /dag wanted down until they are gone" retiredNodesAreDown
        , testCase "sync, async and stdin leave the same world; sync answers with the line's reports" syncAndAsyncAgree
        , testCase "reads answer while the loop is inside a long up" readsDuringLongUp
        , testCase "/help/seed, /history and the error responses" theOtherReads
        , testCase "/dag carries the mode the loop's accessor answers at the moment of the read" dagCarriesMode
        , testCase "GET / is the web UI's page, /ui/* its files, and a missing one is 404" theWebUi
        ]

-------------------------------------------------------------------------------
-- the thing served: counters per node name, plus one node that blocks

newtype Spec = Spec {specNames :: [String]}
    deriving (Eq, Show, Generic)

instance ToJSON Spec
instance FromJSON Spec

parseSpec :: [String] -> Either Text Spec
parseSpec [] = Left "expected at least one node name"
parseSpec args = Right (Spec args)

-- | The name of the node whose @up@ waits to be let go.
slowName :: String
slowName = "slow"

data Slow = Slow
    { slowStarted :: MVar ()
    -- ^ filled when the slow node's @up@ is entered
    , slowGate :: MVar ()
    -- ^ filled by the test to let it return
    }

spyProgram :: Slow -> IORef (Map String Int) -> IORef (Map String Int) -> Track' Spec
spyProgram slow upsRef downsRef = Track $ \spec ->
    op "http-root" (deps (fmap nodeOp spec.specNames)) $ \actions ->
        actions{ref = mkRef "http-root" spec.specNames, help = "the root of " <> Text.pack (unwords spec.specNames)}
  where
    nodeOp name =
        op "http-node" nodeps $ \actions ->
            actions
                { ref = mkRef "http-node" name
                , help = "node " <> Text.pack name
                , up =
                    if name == slowName
                        then putMVar (slowStarted slow) () >> takeMVar (slowGate slow)
                        else bump upsRef name
                , down = bump downsRef name
                }
    bump r name = atomicModifyIORef' r (\m -> (Map.insertWith (+) name 1 m, ()))

-------------------------------------------------------------------------------
-- a running loop with an HTTP server

data Running = Running
    { runningStdin :: Handle
    -- ^ the writing end of the loop's standard input; closing it ends the loop
    , runningWorld :: MVar (World Spec Spec)
    , runningOwn :: IO [Tagged.Tagged]
    -- ^ everything the loop's own reporter saw
    , runningUps :: IORef (Map String Int)
    , runningDowns :: IORef (Map String Int)
    , runningSlow :: Slow
    , runningManager :: HTTP.Manager
    }

seedHelp :: Text
seedHelp = "usage: config NAME...\n"

{- | Start the loop on a temp socket with a pipe for standard input, hand
it to the test, and make sure it has ended before the temp dir goes.
-}
withRunning :: (Running -> IO a) -> IO a
withRunning = withRunningMode (pure Serve.Interactive)

-- | 'withRunning' with the server's mode accessor chosen by the test.
withRunningMode :: IO Serve.Mode -> (Running -> IO a) -> IO a
withRunningMode mode act =
    withTempDir $ \dir -> do
        let path = dir </> "serve.http"
        (stdinR, stdinW) <- createPipe
        worldVar <- newEmptyMVar
        (own, seen) <- capture
        upsRef <- newIORef Map.empty
        downsRef <- newIORef Map.empty
        slow <- Slow <$> newEmptyMVar <*> newEmptyMVar
        Http.withHttpServer path seedHelp mode $ \server -> do
            let base = (contramap attributed (Tagged.serveStream own), contramap attributed (Tagged.updownStream own))
                (serveR, updownR) = Http.serverReporters server base
            _ <- forkIO $ do
                w <-
                    Serve.serveObserved
                        (Http.serverObserver server)
                        []
                        Nothing
                        True
                        serveR
                        updownR
                        parseSpec
                        (Configure pure)
                        (spyProgram slow upsRef downsRef)
                        Nothing
                        [Serve.stdinProducer stdinR, Http.serverProducer server]
                putMVar worldVar w
            manager <- unixManager path
            let running = Running stdinW worldVar seen upsRef downsRef slow manager
            r <- act running
            _ <- try (hClose stdinW) :: IO (Either IOError ())
            _ <- awaitWorld running
            pure r

awaitWorld :: Running -> IO (World Spec Spec)
awaitWorld running = do
    mw <- timeout (10 * 1000000) (takeMVar (runningWorld running))
    case mw of
        Nothing -> assertFailure "the loop did not end"
        Just w -> do
            putMVar (runningWorld running) w
            pure w

-- | End the loop through standard input and hand back the world it left.
finish :: Running -> IO (World Spec Spec)
finish running = do
    hClose (runningStdin running)
    awaitWorld running

-------------------------------------------------------------------------------
-- an http client over the unix socket

unixManager :: FilePath -> IO HTTP.Manager
unixManager path =
    HTTP.newManager
        HTTP.defaultManagerSettings
            { HTTP.managerRawConnection = pure $ \_ _ _ -> do
                sock <- Socket.socket Socket.AF_UNIX Socket.Stream Socket.defaultProtocol
                Socket.connect sock (Socket.SockAddrUnix path)
                makeConnection (SocketBS.recv sock 4096) (SocketBS.sendAll sock) (Socket.close sock)
            }

-- | A GET, decoded; the status and the body.
get :: Running -> String -> IO (Int, Value)
get running route = do
    req <- HTTP.parseRequest ("http://salmon" <> route)
    exchange running req

-- | A text @POST \/command@, decoded.
post :: Running -> String -> String -> IO (Int, Value)
post running route line = do
    req0 <- HTTP.parseRequest ("http://salmon" <> route)
    let req =
            req0
                { HTTP.method = "POST"
                , HTTP.requestHeaders = [(HTTP.hContentType, "text/plain")]
                , HTTP.requestBody = HTTP.RequestBodyLBS (LChar8.pack line)
                }
    exchange running req

-- | A GET left undecoded: the status, the content type, and the body.
getRaw :: Running -> String -> IO (Int, Maybe LChar8.ByteString, LChar8.ByteString)
getRaw running route = do
    req <- HTTP.parseRequest ("http://salmon" <> route)
    r <- timeout (10 * 1000000) (HTTP.httpLbs req (runningManager running))
    case r of
        Nothing -> assertFailure ("no answer within 10s to " <> route)
        Just resp ->
            pure
                ( HTTP.statusCode (HTTP.responseStatus resp)
                , LChar8.fromStrict <$> lookup HTTP.hContentType (HTTP.responseHeaders resp)
                , HTTP.responseBody resp
                )

exchange :: Running -> HTTP.Request -> IO (Int, Value)
exchange running req = do
    r <- timeout (10 * 1000000) (HTTP.httpLbs req (runningManager running))
    case r of
        Nothing -> assertFailure ("no answer within 10s to " <> show (HTTP.path req))
        Just resp ->
            case eitherDecode (HTTP.responseBody resp) of
                Left err -> assertFailure ("not JSON: " <> err <> ": " <> LChar8.unpack (HTTP.responseBody resp))
                Right v -> pure (HTTP.statusCode (HTTP.responseStatus resp), v)

-- | A synchronous command: the kinds of the reports it answered with.
sync :: Running -> String -> IO [Text]
sync running line = do
    (code, v) <- post running "/command" line
    assertEqual ("status of sync " <> line) 200 code
    case v of
        Array xs -> pure (fmap kindOf (toList xs))
        _ -> assertFailure ("sync answer is not an array: " <> show v)

-- | An asynchronous command: the sequence number it was queued at.
async :: Running -> String -> IO Integer
async running line = do
    (code, v) <- post running "/command?async" line
    assertEqual ("status of async " <> line) 202 code
    case v of
        Object o | Just (Number n) <- KeyMap.lookup "seq" o -> pure (truncate n)
        _ -> assertFailure ("async answer has no seq: " <> show v)

-------------------------------------------------------------------------------
-- reading the JSON

kindOf :: Value -> Text
kindOf v = maybe "<no kind>" id (textAt ["kind"] v)

textAt :: [Text] -> Value -> Maybe Text
textAt [] (String t) = Just t
textAt (k : ks) (Object o) = KeyMap.lookup (Key.fromText k) o >>= textAt ks
textAt _ _ = Nothing

field :: Text -> Value -> Maybe Value
field k (Object o) = KeyMap.lookup (Key.fromText k) o
field _ _ = Nothing

withoutSeq :: Value -> Value
withoutSeq (Object o) = Object (KeyMap.delete "seq" o)
withoutSeq v = v

arrayOf :: Value -> [Value]
arrayOf (Array xs) = toList xs
arrayOf _ = []

-- | The @nodes@ of a @\/dag@ answer.
dagNodes :: Value -> IO [Value]
dagNodes v = case field "nodes" v of
    Just (Array xs) -> pure (toList xs)
    _ -> assertFailure ("/dag without nodes: " <> show v)

-- | The lines 'Help.printDagTree' would print, rebuilt from a @\/dag@ answer.
dagLinesOf :: [Value] -> [Text]
dagLinesOf nodes = concatMap nodeLines nodes
  where
    shorthandByRef :: Map Text Text
    shorthandByRef = Map.fromList (mapMaybe (\n -> (,) <$> textAt ["ref", "full"] n <*> textAt ["shorthand"] n) nodes)
    nodeLines n =
        let sh = orNothing (textAt ["shorthand"] n)
            full = orNothing (textAt ["ref", "full"] n)
            hlp = orNothing (textAt ["help"] n)
            depLine d =
                let dref = orNothing (textAt ["full"] d)
                 in "  <- " <> Map.findWithDefault dref dref shorthandByRef
         in (sh <> " (" <> full <> ") " <> hlp) : fmap depLine (maybe [] arrayOf (field "dependencies" n))
    orNothing = maybe "<missing>" id

-------------------------------------------------------------------------------

dagMatchesPrintDagTree :: IO ()
dagMatchesPrintDagTree =
    withRunning $ \running -> do
        _ <- sync running "supervise off"
        _ <- sync running "up n1 n2"
        _ <- sync running "up n2 n3"
        (code, v) <- get running "/dag"
        assertEqual "status" 200 code
        nodes <- dagNodes v
        w <- finish running
        let dag = Serve.worldDag w
        assertEqual "the same lines printDagTree prints" (Help.dagLines dag) (dagLinesOf nodes)
        assertEqual "one node per world node" (Map.size w.worldNodes) (length nodes)
        -- edges in both directions agree: every dependency edge is a
        -- dependant edge on the other node, and vice versa
        let refOf n = orMissing (textAt ["ref", "full"] n)
            edgesOut = Set.fromList [(orMissing (textAt ["full"] d), refOf n) | n <- nodes, d <- maybe [] arrayOf (field "dependencies" n)]
            edgesIn = Set.fromList [(refOf n, orMissing (textAt ["full"] d)) | n <- nodes, d <- maybe [] arrayOf (field "dependants" n)]
        assertEqual "dependants are the transpose of dependencies" edgesOut edgesIn
        assertBool "there are edges" (not (Set.null edgesOut))
        -- and every node carries the loop's state and the representative's fields
        forM_ nodes $ \n -> do
            assertEqual ("direction of " <> show (refOf n)) (Just "up") (textAt ["direction"] n)
            assertEqual ("convergence of " <> show (refOf n)) (Just "converged") (textAt ["convergence"] n)
            assertBool "notes present" (field "notes" n /= Nothing)
            assertBool "dynamics present" (field "dynamics" n /= Nothing)
            assertBool "status present (null: never tended)" (field "status" n /= Nothing)
  where
    orMissing = maybe "<missing>" id

dagBeforeAnyPass :: IO ()
dagBeforeAnyPass =
    withRunning $ \running -> do
        _ <- sync running "supervise off"
        _ <- sync running "autoconverge off"
        declared <- sync running "up n1 n2"
        assertEqual "no pass ran" ["declared"] declared
        (_, v) <- get running "/dag"
        nodes <- dagNodes v
        assertEqual "three nodes, no pass" 3 (length nodes)
        forM_ nodes $ \n -> do
            assertEqual "pending" (Just "pending") (textAt ["convergence"] n)
            assertEqual "up" (Just "up") (textAt ["direction"] n)
        ups <- readIORef (runningUps running)
        assertEqual "nothing was applied" Map.empty ups
        converged <- sync running "converge"
        assertBool ("the pass ran: " <> show converged) ("converge-stop" `elem` converged)
        (_, v') <- get running "/dag"
        nodes' <- dagNodes v'
        forM_ nodes' $ \n -> assertEqual "converged" (Just "converged") (textAt ["convergence"] n)

retiredNodesAreDown :: IO ()
retiredNodesAreDown =
    withRunning $ \running -> do
        _ <- sync running "supervise off"
        _ <- sync running "up n1 n2"
        _ <- sync running "autoconverge off"
        _ <- sync running "down n1 n2"
        (_, v) <- get running "/dag"
        nodes <- dagNodes v
        assertEqual "still there, on their way down" 3 (length nodes)
        forM_ nodes $ \n -> do
            assertEqual "down" (Just "down") (textAt ["direction"] n)
            assertEqual "pending" (Just "pending") (textAt ["convergence"] n)
        -- the edges survive the retraction: they are what orders the teardown
        assertBool "edges kept" (any (\n -> not (null (maybe [] arrayOf (field "dependencies" n)))) nodes)
        _ <- sync running "converge"
        (_, v') <- get running "/dag"
        nodes' <- dagNodes v'
        assertEqual "gone once down" 0 (length nodes')
        downs <- readIORef (runningDowns running)
        assertEqual "both nodes went down" (Map.fromList [("n1", 1), ("n2", 1)]) downs

syncAndAsyncAgree :: IO ()
syncAndAsyncAgree = do
    (stdinUps, stdinDowns, stdinWorld, stdinKinds) <- runScript script
    -- synchronously: each line answered with its reports
    (syncUps, syncDowns, syncWorld, syncKinds) <- withRunning $ \running -> do
        kinds <- concat <$> forM script (sync running)
        w <- finish running
        (,,,) <$> readIORef (runningUps running) <*> readIORef (runningDowns running) <*> pure w <*> pure kinds
    -- as a multiset: a convergence pass is concurrent, so two nodes ready
    -- at the same time report in whichever order their threads ran, on
    -- stdin and over HTTP alike
    assertEqual "sync answers with exactly the reports the lines produced on stdin" (sort stdinKinds) (sort syncKinds)
    assertEqual "the first line's reports come first" (take 2 stdinKinds) (take 2 syncKinds)
    -- asynchronously: queued, then a sync `status` that is handled after them all
    (asyncUps, asyncDowns, asyncWorld) <- withRunning $ \running -> do
        seqs <- forM script (async running)
        assertBool ("sequence numbers are assigned at enqueue, in order: " <> show seqs) (and (zipWith (<) seqs (drop 1 seqs)))
        _ <- sync running "status"
        own <- runningOwn running
        let handled = length (filter isConvergeStop own)
        assertEqual "every declaring line was handled before the status" (length (filter isDeclaring script)) handled
        w <- finish running
        (,,) <$> readIORef (runningUps running) <*> readIORef (runningDowns running) <*> pure w
    assertEqual "ups (sync)" stdinUps syncUps
    assertEqual "downs (sync)" stdinDowns syncDowns
    assertEqual "world (sync)" (worldShape stdinWorld) (worldShape syncWorld)
    assertEqual "ups (async)" stdinUps asyncUps
    assertEqual "downs (async)" stdinDowns asyncDowns
    assertEqual "world (async)" (worldShape stdinWorld) (worldShape asyncWorld)
  where
    script = ["supervise off", "up n1", "up n1 n2", "down n1", "history"]
    isDeclaring l = any (`Text.isPrefixOf` Text.pack l) ["up ", "down "]
    isConvergeStop t = case t of
        Tagged.FromServe Serve.ConvergeStop{} -> True
        _ -> False

readsDuringLongUp :: IO ()
readsDuringLongUp =
    withRunning $ \running -> do
        _ <- sync running "supervise off"
        _ <- sync running "up n1"
        seqNo <- async running ("up " <> slowName)
        assertBool "a later number than the earlier commands'" (seqNo >= 2)
        -- the loop is now inside the slow node's up
        started <- timeout (10 * 1000000) (takeMVar (slowStarted (runningSlow running)))
        assertEqual "the slow up was entered" (Just ()) started
        -- and reads answer without waiting for it
        (code, v) <- get running "/status"
        assertEqual "status answers" 200 code
        let nodes = maybe [] arrayOf (field "nodes" v)
        assertEqual "the earlier seed's nodes plus the slow seed's" 4 (length nodes)
        let slowNode = [n | n <- nodes, textAt ["help"] n == Just ("node " <> Text.pack slowName)]
        assertEqual "the slow node is still pending" [Just "pending"] (fmap (textAt ["convergence"]) slowNode)
        (code', v') <- get running "/dag"
        assertEqual "dag answers" 200 code'
        nodes' <- dagNodes v'
        assertEqual "dag has the same nodes" 4 (length nodes')
        (code'', _) <- get running "/history"
        assertEqual "history answers" 200 code''
        -- let it go, and make sure the loop is past it before ending
        putMVar (slowGate (runningSlow running)) ()
        _ <- sync running "status"
        (_, after) <- get running "/status"
        let slowAfter = [n | n <- maybe [] arrayOf (field "nodes" after), textAt ["help"] n == Just ("node " <> Text.pack slowName)]
        assertEqual "the slow node converged once let go" [Just "converged"] (fmap (textAt ["convergence"]) slowAfter)

theOtherReads :: IO ()
theOtherReads =
    withRunning $ \running -> do
        _ <- sync running "supervise off"
        _ <- sync running "up n1"
        _ <- sync running "down n1"
        (code, h) <- get running "/help/seed"
        assertEqual "help status" 200 code
        assertEqual "the seed help is the text the binary supplied" (Just seedHelp) (textAt ["seed"] h)
        assertBool "the command reference is there" (not (null (maybe [] arrayOf (field "commands" h))))
        (hcode, hist) <- get running "/history"
        assertEqual "history status" 200 hcode
        assertEqual "kind" (Just "history") (textAt ["kind"] hist)
        assertEqual "two declarations" 2 (length (maybe [] arrayOf (field "seeds" hist)))
        assertEqual "nothing elided" (Just (Number 0)) (field "elided" hist)
        (scode, st) <- get running "/status"
        assertEqual "status status" 200 scode
        assertEqual "kind" (Just "status") (textAt ["kind"] st)
        -- and the same object the loop's own `status` produces, plus the
        -- event stream's cursor (milestone 4)
        assertBool "the read carries seq" (isJust (field "seq" st))
        _ <- sync running "status"
        own <- runningOwn running
        let fromLoop = [toJSON t | t@(Tagged.FromServe Serve.StatusReport{}) <- own]
        assertEqual "the same object --json prints" [withoutSeq st] fromLoop
        (nf, _) <- get running "/nope"
        assertEqual "unknown route" 404 nf
        (mna, _) <- post running "/dag" "status"
        assertEqual "wrong method" 405 mna
        (bad, _) <- post running "/command" "status\nhistory"
        assertEqual "two lines in one body" 400 bad

theWebUi :: IO ()
theWebUi =
    withRunning $ \running -> do
        (code, ctype, body) <- getRaw running "/"
        assertEqual "the page's status" 200 code
        assertEqual "the page's content type" (Just "text/html; charset=utf-8") ctype
        assertBool "the page is HTML" ("<!doctype html>" `LChar8.isPrefixOf` body)
        assertBool "the page loads the script" ("ui/ui.js" `isInfixOf` LChar8.unpack body)
        (jcode, jtype, js) <- getRaw running "/ui/ui.js"
        assertEqual "the script's status" 200 jcode
        assertEqual "the script's content type" (Just "text/javascript; charset=utf-8") jtype
        assertBool "the script subscribes from the snapshot's seq" ("events?since=" `isInfixOf` LChar8.unpack js)
        assertBool "the script's writes are asynchronous commands" ("command?async" `isInfixOf` LChar8.unpack js)
        assertBool "the script's seed form reads the seed's help" ("help/seed" `isInfixOf` LChar8.unpack js)
        assertBool "the script never sends quit" (not ("post(\"quit\"" `isInfixOf` LChar8.unpack js) && not ("data-line=\"quit\"" `isInfixOf` LChar8.unpack body))
        (ccode, ctype', _) <- getRaw running "/ui/ui.css"
        assertEqual "the stylesheet's status" 200 ccode
        assertEqual "the stylesheet's content type" (Just "text/css; charset=utf-8") ctype'
        (missing, _) <- get running "/ui/missing"
        assertEqual "a file outside the embedded set" 404 missing
        (mna, _) <- post running "/" "status"
        assertEqual "the page takes no POST" 405 mna
        _ <- finish running
        pure ()

-------------------------------------------------------------------------------

-- | The same script on one stdin, for the comparison: side effects, world,
-- and the kinds of every report the loop produced for its lines.
runScript :: [String] -> IO (Map String Int, Map String Int, World Spec Spec, [Text])
runScript script = do
    upsRef <- newIORef Map.empty
    downsRef <- newIORef Map.empty
    slow <- Slow <$> newEmptyMVar <*> newEmptyMVar
    (own, seen) <- capture
    w <- withScript script $ \h ->
        Serve.serveWith [] Nothing True (Tagged.serveStream own) (Tagged.updownStream own) parseSpec (Configure pure) (spyProgram slow upsRef downsRef) h
    reports <- seen
    let kinds = [k | t <- reports, let k = kindOf (toJSON t), k `notElem` ["started", "stopped"]]
    (,,,) <$> readIORef upsRef <*> readIORef downsRef <*> pure w <*> pure kinds

withScript :: [String] -> (Handle -> IO a) -> IO a
withScript ls act =
    withSystemTempFile "salmon-serve-http-script" $ \path h -> do
        hPutStr h (unlines ls)
        hClose h
        withFile path ReadMode act

-- | The comparable part of a 'World', as 'Test.ServeSocketSpec' takes it.
worldShape :: World Spec Spec -> (Map Ref (Direction, Convergence), Set.Set Ref, Int, Int, Int)
worldShape w =
    ( Map.map (\st -> (st.nodeDirection, st.nodeConvergence)) w.worldNodes
    , Map.keysSet w.worldMagma
    , Map.size w.worldLedger
    , length w.worldEpochs
    , length w.worldLog
    )


{- | The @mode@ on @\/dag@ is read from the server's accessor at the moment
of the read, not fixed when the server starts: the same accessor is what
@\/status@ opens with, and the loop's mode moves (@replay@ turns to
@following@ at the first full round, "Salmon.Actions.Follow").
-}
dagCarriesMode :: IO ()
dagCarriesMode = do
    modeRef <- newIORef Serve.Replay
    withRunningMode (readIORef modeRef) $ \running -> do
        _ <- sync running "supervise off"
        _ <- sync running "up n1"
        (code, v) <- get running "/dag"
        assertEqual "status" 200 code
        assertEqual "the mode the accessor answered" (Just "replay") (textAt ["mode"] v)
        (_, st) <- get running "/status"
        assertEqual "/status agrees" (Just "replay") (textAt ["mode"] st)
        atomicModifyIORef' modeRef (const (Serve.Following, ()))
        (_, v') <- get running "/dag"
        assertEqual "the mode after the accessor moved" (Just "following") (textAt ["mode"] v')
        (_, st') <- get running "/status"
        assertEqual "/status moved with it" (Just "following") (textAt ["mode"] st')
        _ <- finish running
        pure ()
