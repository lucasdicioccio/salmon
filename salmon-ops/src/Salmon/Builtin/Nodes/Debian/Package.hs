module Salmon.Builtin.Nodes.Debian.Package where

import Salmon.Builtin.Extension
import qualified Salmon.Builtin.Nodes.Binary as Binary
import Salmon.Op.OpGraph
import Salmon.Op.Ref
import Salmon.Op.Actions (Act (..))
-- only `addEdge` is needed here; `Rewritten` carries the `Dag` itself.
import qualified Salmon.Op.Dag as Dag
import Salmon.Op.Rewrite (Phase (..), Rewrite, Rewritten)
import qualified Salmon.Op.Rewrite as Rewrite
import Salmon.Reporter

import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar, readMVar)
import Data.Dynamic (toDyn)
import Data.Foldable (toList)
import qualified Data.List as List
import qualified Data.List.NonEmpty as NEList
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
import GHC.IO.Exception (ExitCode (..))
import System.Environment (getEnvironment)
import System.IO.Unsafe (unsafePerformIO)
import System.Process.ByteString (readCreateProcessWithExitCode)
import System.Process.ListLike (CreateProcess, env, proc)
import qualified Data.Text.Encoding as Text

import Salmon.Actions.UpDown (CheckResult (..))

data Package = Package {pkgName :: Text}
    deriving (Eq, Ord, Show)

-------------------------------------------------------------------------------

-- | Which apt-get invocation a 'Report' is for (including the full package set, e.g. to see why an install failed with "too many arguments").
data AptCommand
    = AptInstall !(NEList.NonEmpty Package)
    | -- | the index refresh, on behalf of these packages
      AptUpdate !(NEList.NonEmpty Package)
    | AptRemove !(NEList.NonEmpty Package)
    deriving (Show)

data Report
    = RunAptGet !AptCommand !Binary.Report
    deriving (Show)

-------------------------------------------------------------------------------

deb :: Package -> Op
deb = debWith silent

-- | Like 'deb', but takes a 'Reporter' to observe the apt-get invocation (command, exit code, stdout/stderr).
debWith :: Reporter Report -> Package -> Op
debWith r pkg =
    op "deb" (deps [aptIndexWith r pkgs]) $ \actions ->
        actions
            { help = "installs " <> pkg.pkgName
            , ref = mkRef "debian-deb" pkg.pkgName
            , up = upAction
            , down = downAction
            , check = checkPackagesInstalled pkgs
            , dynamics = [toDyn pkg]
            }
  where
    pkgs :: NEList.NonEmpty Package
    pkgs = NEList.singleton pkg

    upAction :: IO ()
    upAction = do
        baseEnv <- getEnvironment
        Binary.untrackedExec (aptInstallCommand baseEnv) pkgs "" (contramap (RunAptGet (AptInstall pkgs)) r)
    downAction :: IO ()
    downAction =
        Binary.untrackedExec aptUninstallCommand pkgs "" (contramap (RunAptGet (AptRemove pkgs)) r)

debs :: NEList.NonEmpty Package -> Op
debs = debsWith silent

-- | Like 'debs', but takes a 'Reporter' to observe the apt-get invocation (command, exit code, stdout/stderr).
debsWith :: Reporter Report -> NEList.NonEmpty Package -> Op
debsWith r pkgs =
    op "debs" (deps [aptIndexWith r dedupedPkgs]) $ \actions ->
        actions
            { help = "installs " <> Text.pack (show (length pkgset)) <> " packages"
            , notes = pkgName <$> toList pkgset
            , ref = mkRef "debian-deb-set" (pkgName <$> Set.toList pkgset)
            , up = upAction
            , down = downAction
            , check = checkPackagesInstalled dedupedPkgs
            }
  where
    pkgset :: Set.Set Package
    pkgset = Set.fromList $ NEList.toList pkgs
    -- dedup before ever building the apt-get argv, not just for display (`help`/`notes`/`ref` above) —
    -- otherwise many predecessors depending on the same package (e.g. one per migration file) each
    -- contribute their own copy of it to the same apt-get invocation.
    dedupedPkgs :: NEList.NonEmpty Package
    dedupedPkgs = NEList.fromList $ Set.toList pkgset
    upAction :: IO ()
    upAction = do
        baseEnv <- getEnvironment
        Binary.untrackedExec (aptInstallCommand baseEnv) dedupedPkgs "" (contramap (RunAptGet (AptInstall dedupedPkgs)) r)
    downAction :: IO ()
    downAction =
        Binary.untrackedExec aptUninstallCommand dedupedPkgs "" (contramap (RunAptGet (AptRemove dedupedPkgs)) r)

-------------------------------------------------------------------------------

{- | What an 'aptIndex' node carries on @dynamics@: the packages it makes sure
apt has heard of. 'batchPackages' collects these the way it collects
'Package's, so a batched graph refreshes the index once.
-}
newtype AptIndexFor = AptIndexFor [Package]
    deriving (Eq, Ord, Show)

{- | The apt index knowing how to install these packages: the node 'deb' and
'debs' depend on, and the reason they work on a machine whose index has never
been refreshed.

A fresh cloud image boots with whatever index the image was built with --
often none, or @main@ without @universe@ -- and @apt-get install@ then fails
with "has no installation candidate" for some packages and succeeds for
others, on one machine in one state. So before any install, this node asks
whether apt has a /candidate/ for each package and runs @apt-get update@ when
it does not.

= When it does nothing

The check ('checkAptIndex') is satisfied, and @apt-get update@ is not run,
when either holds:

* every package is __already installed__ ('checkPackagesInstalled') -- an
  index is only needed to install something, and this is what keeps a graph
  naming packages it already has runnable as an ordinary user;
* @apt-cache policy@ reports a __candidate__ for every one of them. That is
  the case on any machine whose index is in ordinary use, so there an install
  costs one @apt-cache@ call more than it did and no refresh.

Note what it is /not/: a freshness guarantee. An index that knows an old
version of a package is good enough to install that package, and "track the
latest" is no more this node's job than it is 'deb''s.

= Virtual names and names nobody has

@apt-cache policy@ says @Candidate: (none)@ for a purely virtual package
(@ssh-client@) even on a fresh index, and nothing at all for a name it has
never heard of. Neither can be told from "the index is stale", so both cost
one refresh -- after which this node has done all it can, and whether the
name installs is 'deb''s to report, with apt's own message. To keep a
supervisor from re-running @apt-get update@ at its delay floor over such a
name, a refresh this process ran less than 'aptIndexFreshFor' ago satisfies
the check whatever @apt-cache@ says.

= Concurrency

Refreshes are serialised inside the process, and the question is asked again
under the lock: of twenty of these becoming ready at once on a fresh machine
(which @run serve@ does, without 'batchPackages'), one runs @apt-get update@
and nineteen find their candidates. Another /process/ holding apt's lists
lock still fails the command, and the node with it.

= Beside an external repository

A @deb@ given a "Salmon.Builtin.Nodes.Debian.AptRepository" has that node and
this one as two dependencies with no order between them. The repository's own
@apt-get update@ goes through 'recordingAptIndexRefresh', so the two never run
at once and a refresh by either answers for both. Where this node happens to
go first for a package only the repository carries, it runs one refresh that
finds nothing -- once, on the pass that adds the repository.

= down

Nothing. A refreshed index is not something to take back.
-}
aptIndex :: NEList.NonEmpty Package -> Op
aptIndex = aptIndexWith silent

-- | Like 'aptIndex', but takes a 'Reporter' to observe the @apt-get update@.
aptIndexWith :: Reporter Report -> NEList.NonEmpty Package -> Op
aptIndexWith r pkgs =
    op "apt-index" nodeps $ \actions ->
        actions
            { help = "refreshes apt's package lists unless they already offer " <> describe names
            , notes = names
            , ref = mkRef "debian-apt-index" names
            , up = refreshAptIndex r dedupedPkgs
            , check = checkAptIndex dedupedPkgs
            , dynamics = [toDyn (AptIndexFor (toList dedupedPkgs))]
            }
  where
    dedupedPkgs :: NEList.NonEmpty Package
    dedupedPkgs = NEList.fromList (Set.toList (Set.fromList (toList pkgs)))
    names :: [Text]
    names = pkgName <$> toList dedupedPkgs
    describe [one] = one
    describe many = Text.pack (show (length many)) <> " packages"

-- | How long a refresh run by this process answers for: see 'aptIndex'.
aptIndexFreshFor :: NominalDiffTime
aptIndexFreshFor = 3600

{- | When this process last ran @apt-get update@ successfully, and the lock
that serialises doing so. Process-global because the thing it guards is: there
is one apt index per machine, however many nodes and graphs ask after it.
-}
aptIndexRefreshed :: MVar (Maybe UTCTime)
aptIndexRefreshed = unsafePerformIO (newMVar Nothing)
{-# NOINLINE aptIndexRefreshed #-}

{- | Run something that refreshes the apt index -- an @apt-get update@ -- with
no other such refresh of this process running, and remember that it happened.
For any node that refreshes the index on its own account.
-}
recordingAptIndexRefresh :: IO () -> IO ()
recordingAptIndexRefresh act =
    modifyMVar_ aptIndexRefreshed $ \_ -> act >> (Just <$> getCurrentTime)

refreshedRecently :: Maybe UTCTime -> IO Bool
refreshedRecently Nothing = pure False
refreshedRecently (Just at) = do
    now <- getCurrentTime
    pure (diffUTCTime now at < aptIndexFreshFor)

checkAptIndex :: NEList.NonEmpty Package -> IO CheckResult
checkAptIndex pkgs = do
    recent <- refreshedRecently =<< readMVar aptIndexRefreshed
    checkAptIndexGiven recent pkgs

checkAptIndexGiven :: Bool -> NEList.NonEmpty Package -> IO CheckResult
checkAptIndexGiven recent pkgs = do
    installed <- checkPackagesInstalled pkgs
    case installed of
        Success -> pure Success
        _ -> do
            baseEnv <- getEnvironment
            -- LC_ALL=C: "Candidate:" is a translated string.
            (code, out, _err) <-
                readCreateProcessWithExitCode
                    (proc "apt-cache" ("policy" : names)){env = Just (("LC_ALL", "C") : filter ((/= "LC_ALL") . fst) baseEnv)}
                    ""
            pure $ interpretAptPolicy recent (fmap Text.pack names) code (Text.decodeUtf8 out)
  where
    names = [Text.unpack pkg.pkgName | pkg <- toList pkgs]

{- | The verdict drawn from @apt-cache policy NAME...@, split out for
testability. The first argument is whether this process refreshed the index
recently, in which case there is nothing more a refresh could do.

@apt-cache policy@ and not @apt-cache show@: the latter succeeds for a name
another package merely refers to. A name with no stanza at all, or one whose
stanza says @Candidate: (none)@, has no candidate.
-}
interpretAptPolicy :: Bool -> [Text] -> ExitCode -> Text -> CheckResult
interpretAptPolicy _ _ (ExitFailure _) _ =
    -- could not ask; not evidence either way.
    Unknown
interpretAptPolicy recent wanted ExitSuccess out =
    case filter (not . (`Set.member` candidates)) wanted of
        [] -> Success
        missing
            | recent -> Success
            | otherwise -> Failure ("no installation candidate in the apt index for: " <> Text.intercalate ", " missing)
  where
    candidates :: Set Text
    candidates = Set.fromList (go Nothing (Text.lines out))

    go :: Maybe Text -> [Text] -> [Text]
    go _ [] = []
    go current (line : rest)
        -- a stanza opens with "name:" at column 0; everything under it is indented
        | Just name <- Text.stripSuffix ":" line
        , not (Text.null name)
        , not (Text.isPrefixOf " " line) =
            go (Just name) rest
        | Just name <- current
        , Just value <- Text.stripPrefix "Candidate:" (Text.strip line)
        , Text.strip value /= "(none)"
        , not (Text.null (Text.strip value)) =
            name : go Nothing rest
        | otherwise = go current rest

refreshAptIndex :: Reporter Report -> NEList.NonEmpty Package -> IO ()
refreshAptIndex r pkgs =
    modifyMVar_ aptIndexRefreshed $ \lastRefresh -> do
        -- asked again under the lock: whoever held it may have just done this.
        recent <- refreshedRecently lastRefresh
        verdict <- checkAptIndexGiven recent pkgs
        case verdict of
            Success -> pure lastRefresh
            _ -> do
                baseEnv <- getEnvironment
                Binary.untrackedExec (aptUpdateCommand baseEnv) () "" (contramap (RunAptGet (AptUpdate pkgs)) r)
                Just <$> getCurrentTime

{- | Collect every @deb@ node in the graph into one @apt-get@ invocation per
direction: one install batch for the packages some live declaration still
wants, one removal batch for the rest, and an ordering edge putting the
removal first.

This replaces the 'installAllDebsAtOnce' \/ 'removeSinglePackages' pair of
@Op -> Op@ passes an application used to apply by hand inside its own
'Salmon.Op.Track.Track' (both still work, both deprecated). Three things it
can do that they could not, all of them consequences of running after the fold
rather than over one directive's graph:

* __It sees every declaration.__ Under @run serve@ the old pass batched one
  seed's packages at a time, because that is all a @directive -> Op@ ever
  had. This batches across the lot.
* __It knows the direction.__ 'phaseDesired' is what says whether a @deb@
  node is being installed or removed, and nothing before the fold knows
  that — so the old pass could only ever emit a blind install batch. The
  partition here is conservative: a package any live declaration still wants
  goes to the install batch, and only a package absent from 'phaseDesired'
  is removed. Erring the other way would let one retraction uninstall a
  package another declaration is standing on.
* __It redirects the edges.__ Whatever depended on @deb foo@ now depends on
  the batch that installs it, instead of the old pass's trick of blanking the
  per-package nodes and 'Salmon.Op.OpGraph.inject'ing the batch under the
  root.

The ordering edge is there because @apt-get install@ and @apt-get remove@
both want the dpkg lock. Today the two batches are in different convergence
passes anyway, so the edge is redundant; once nodes run concurrently it is
what serialises them, and an edge costs nothing and needs no retry loop to
tell "could not lock" from "no such package". Removals first is also simply
the right order — it is what one would do by hand to clear conflicts.

The 'aptIndex' nodes those @deb@ nodes depend on are collected the same way,
into one node asking after every wanted package at once, which the install
batch then depends on (it inherits its members' dependencies). Only the
wanted ones: an index node has nothing to do on the way down, so the rest are
left as declared.

A batch is one node, so a failure is attributed to all of its members: the
batch's @apt-get@ exiting non-zero says the batch failed, not which package,
and narrowing it would mean parsing apt's prose. That is the trade a
collection makes — efficiency for attribution.
-}
batchPackages :: Reporter Report -> Rewrite Extension
batchPackages r phase computed =
    edge . batchOf "installs" installRef wanted . batchOf "removes" removeRef unwanted . batchIndex $ computed
  where
    -- the index nodes of whatever is wanted up, as one node.
    batchIndex :: Rewritten Extension -> Rewritten Extension
    batchIndex c
        | Just pkgs <- NEList.nonEmpty (Set.toList (Set.fromList (concat [ps | (_, fors) <- indexes, AptIndexFor ps <- fors])))
        , Just act <- opAct (aptIndexWith r pkgs) =
            Rewrite.introduce
                act{extension = act.extension{ref = mkRef "debian-apt-index-batch" (pkgName <$> toList pkgs)}}
                (Set.fromList (fmap fst indexes))
                c
        | otherwise = c

    indexes :: [(Ref, [AptIndexFor])]
    indexes =
        [ (aref, fors)
        | (aref, fors) <- Rewrite.collectDynamic computed
        , not (Set.member aref phase.phaseIgnored)
        , Set.member aref phase.phaseDesired
        ]

    -- (ref, the packages that node declares) for every deb node this
    -- traversal is allowed to touch.
    declared :: [(Ref, [Package])]
    declared =
        [ (aref, pkgs)
        | (aref, pkgs) <- Rewrite.collectDynamic computed
        , not (Set.member aref phase.phaseIgnored)
        ]

    -- conservative: still-wanted wins. Only a package no live declaration
    -- asks for goes to the removal batch.
    wanted, unwanted :: [(Ref, [Package])]
    (wanted, unwanted) = List.partition (\(aref, _) -> Set.member aref phase.phaseDesired) declared

    installRef = batchRef "install" wanted
    removeRef = batchRef "remove" unwanted

    -- removals before installs: both want the dpkg lock, and clearing
    -- conflicts first is the order one would use by hand.
    edge c
        | Map.member installRef (Rewrite.computedMembers c)
        , Map.member removeRef (Rewrite.computedMembers c) =
            c{Rewrite.computedDag = Dag.addEdge (removeRef, installRef) (Rewrite.computedDag c)}
        | otherwise = c

    batchOf :: Text -> Ref -> [(Ref, [Package])] -> Rewritten Extension -> Rewritten Extension
    batchOf verb aref members c
        | Just pkgs <- NEList.nonEmpty (Set.toList (pkgsOf members))
        , Just act <- opAct (debsWith r pkgs) =
            Rewrite.introduce (relabel verb aref (pkgsOf members) act) (Set.fromList (fmap fst members)) c
        | otherwise = c

    -- 'debsWith' already knows how to run one apt-get over a package set; all
    -- this needs of it is a stable identity of its own (so the two batches
    -- are two nodes) and a help line that says which direction it is.
    relabel :: Text -> Ref -> Set Package -> Act Extension -> Act Extension
    relabel verb aref pkgset act =
        act
            { extension =
                act.extension
                    { ref = aref
                    , help = verb <> " " <> Text.pack (show (Set.size pkgset)) <> " packages in one apt-get"
                    }
            }

    pkgsOf :: [(Ref, [Package])] -> Set Package
    pkgsOf members = Set.fromList (concatMap snd members)

    batchRef :: Text -> [(Ref, [Package])] -> Ref
    batchRef what members = mkRef "debian-deb-batch" (what, pkgName <$> Set.toList (pkgsOf members))

{- | The pre-'batchPackages' way of doing this: an @Op -> Op@ an application
applied by hand inside its own 'Salmon.Op.Track.Track', paired with
'removeSinglePackages' to blank the per-package nodes it superseded.

Kept working, but it cannot become direction-aware and it cannot see past one
directive, which is the whole of why 'batchPackages' exists. Porting is:
delete the @optimizedDeps@-style wrapper from the 'Salmon.Op.Track.Track',
and pass @[batchPackages r]@ to
'Salmon.Builtin.CommandLine.execCommandOrSeedWithRewrites'.
-}
installAllDebsAtOnce :: Op -> Op
installAllDebsAtOnce = installAllDebsAtOnceWith silent
{-# DEPRECATED installAllDebsAtOnce "Register `batchPackages` as a rewrite instead; this cannot see other declarations or node directions." #-}

-- | Like 'installAllDebsAtOnce', but takes a 'Reporter' to observe the batched apt-get invocation.
installAllDebsAtOnceWith :: Reporter Report -> Op -> Op
installAllDebsAtOnceWith r =
    collectPackagesAsSet
  where
    collectPackagesAsSet :: Op -> Op
    collectPackagesAsSet root =
        case NEList.nonEmpty (concatMap snd $ collectDynamics root) of
            Just pkgs -> debsWith r pkgs
            Nothing -> realNoop
{-# DEPRECATED installAllDebsAtOnceWith "Register `batchPackages` as a rewrite instead; this cannot see other declarations or node directions." #-}

-- | Blanks every node 'installAllDebsAtOnceWith' has already batched.
removeSinglePackages :: Op -> Op
removeSinglePackages root
    | null (packages root) = root{predecessors = fmap (fmap removeSinglePackages) root.predecessors}
    | otherwise = realNoop{predecessors = fmap (fmap removeSinglePackages) root.predecessors}
  where
    packages :: Op -> [Package]
    packages root = getDynamics root
{-# DEPRECATED removeSinglePackages "Register `batchPackages` as a rewrite instead; it redirects precedence edges rather than blanking nodes." #-}

{- | Whether every one of these packages is already installed.

Written because @apt-get install@ needs root even when it has nothing to do,
so a graph naming packages it already has could not run at all as an ordinary
user -- which is what any local recipe going through
"Salmon.Builtin.Nodes.Self" does, via its @rsync@\/@ssh@ dependencies.

It has to understand __virtual packages__, or it is worse than no check at
all: several names used in this tree ("Salmon.Builtin.Nodes.Debian.OS" asks
for @ssh-client@) are virtual ones that @apt-get@ happily resolves to their
single provider, while @dpkg-query@ answers @not-installed@ for the name
itself forever. So this reads the whole catalogue once and counts a name as
installed when an installed package either /is/ it or @Provides@ it.

The behaviour change is worth stating: a @deb@ node for a package that is
installed but out of date is now skipped rather than handed to @apt-get
install@, which would have upgraded it. "Is this package installed" is what
this node's effect is; tracking the latest version is a different job, and
one nothing in this tree asked for.

One consequence for test harnesses: this check shells out, so it answers
about whatever machine it runs on. Anything redirecting a node's @up@
elsewhere has to redirect the check too, or the check answers about the host
while @up@ acts on the sandbox -- see @Test.PostgresInitSpec@'s shim list,
where leaving @dpkg-query@ out made a container skip an install the host
already had.
-}
checkPackagesInstalled :: NEList.NonEmpty Package -> IO CheckResult
checkPackagesInstalled pkgs = do
    (code, out, _err) <-
        readCreateProcessWithExitCode
            (proc "dpkg-query" ["-W", "-f=${db:Status-Status}|${binary:Package}|${Provides}\n"])
            ""
    pure $ interpretDpkgCatalog (fmap pkgName (toList pkgs)) code (Text.decodeUtf8 out)

{- | The verdict drawn from a @dpkg-query -W@ catalogue of
@status|package|provides@ lines, split out for testability.
-}
interpretDpkgCatalog :: [Text] -> ExitCode -> Text -> CheckResult
interpretDpkgCatalog _ (ExitFailure n) _ =
    -- 'Unknown', not 'Failure': dpkg-query exits non-zero when it cannot read
    -- the status database, which on a live machine mostly means something
    -- else holds the dpkg lock -- unattended-upgrades, typically. That is not
    -- evidence the package is missing, and calling it missing makes the node
    -- run `apt-get install`, which then fails on the same lock. A one-shot
    -- `run up` still applies (Unknown maps to Required), but a supervisor
    -- waits and looks again instead of installing on every busy moment.
    Unknown
interpretDpkgCatalog wanted ExitSuccess catalogue =
    case filter (not . (`Set.member` available)) wanted of
        [] -> Success
        missing -> Failure ("not installed: " <> Text.intercalate ", " missing)
  where
    available :: Set Text
    available = Set.fromList (concatMap namesOf (Text.lines catalogue))

    namesOf :: Text -> [Text]
    namesOf line =
        case Text.splitOn "|" line of
            (status : name : provides : _)
                | Text.strip status == "installed" ->
                    stripArch (Text.strip name) : fmap providedName (Text.splitOn "," provides)
            _ -> []

    -- "libfoo (= 1.2), bar" -> "libfoo" / "bar"
    providedName :: Text -> Text
    providedName = stripArch . Text.strip . Text.takeWhile (/= '(')

    -- dpkg prints "name:arch" for a package from a foreign architecture
    stripArch :: Text -> Text
    stripArch = Text.strip . Text.takeWhile (/= ':')

aptInstallCommand :: [(String, String)] -> Binary.Command "apt-get" (NEList.NonEmpty Package)
aptInstallCommand baseEnv = Binary.Command $ \pkgs -> aptInstallProcess pkgs baseEnv

aptInstallProcess :: NEList.NonEmpty Package -> [(String, String)] -> CreateProcess
aptInstallProcess pkgs baseEnv =
    (proc "apt-get" args){env = Just (("DEBIAN_FRONTEND", "noninteractive") : baseEnv)}
  where
    args :: [String]
    args = ["install", "-y", "-q"] <> [Text.unpack pkg.pkgName | pkg <- toList pkgs]

aptUpdateCommand :: [(String, String)] -> Binary.Command "apt-get" ()
aptUpdateCommand baseEnv =
    Binary.Command $ \() -> (proc "apt-get" ["update", "-q"]){env = Just (("DEBIAN_FRONTEND", "noninteractive") : baseEnv)}

aptUninstallCommand :: Binary.Command "apt-get" (NEList.NonEmpty Package)
aptUninstallCommand = Binary.Command aptUninstallProcess

aptUninstallProcess :: NEList.NonEmpty Package -> CreateProcess
aptUninstallProcess pkgs =
    proc "apt-get" args
  where
    args :: [String]
    args = ["remove", "-q"] <> [Text.unpack pkg.pkgName | pkg <- toList pkgs]
