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

import Data.Dynamic (toDyn)
import Data.Foldable (toList)
import qualified Data.List as List
import qualified Data.List.NonEmpty as NEList
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.IO.Exception (ExitCode (..))
import System.Environment (getEnvironment)
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
    op "deb" nodeps $ \actions ->
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
    op "debs" nodeps $ \actions ->
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

A batch is one node, so a failure is attributed to all of its members: the
batch's @apt-get@ exiting non-zero says the batch failed, not which package,
and narrowing it would mean parsing apt's prose. That is the trade a
collection makes — efficiency for attribution.
-}
batchPackages :: Reporter Report -> Rewrite Extension
batchPackages r phase computed =
    edge . batchOf "installs" installRef wanted . batchOf "removes" removeRef unwanted $ computed
  where
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

aptUninstallCommand :: Binary.Command "apt-get" (NEList.NonEmpty Package)
aptUninstallCommand = Binary.Command aptUninstallProcess

aptUninstallProcess :: NEList.NonEmpty Package -> CreateProcess
aptUninstallProcess pkgs =
    proc "apt-get" args
  where
    args :: [String]
    args = ["remove", "-q"] <> [Text.unpack pkg.pkgName | pkg <- toList pkgs]
