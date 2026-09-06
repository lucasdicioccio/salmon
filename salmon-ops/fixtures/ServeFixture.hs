{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{- | A tiny end-to-end binary for playing with @run serve@
('Salmon.Actions.Serve') by hand, against nothing more dangerous than files
in a directory you name.

A seed is "a named bundle of files under a base directory". The 'Track''
turns it into real 'Salmon.Builtin.Nodes.Filesystem' nodes (a directory plus
one file per name, each with deterministic content), so declaring a seed up
actually creates those files and retiring it actually deletes them — you can
watch the convergence happen in another terminal with @ls@/@watch@.

Because it goes through 'CLI.execCommandOrSeed' it is a full salmon binary,
so the same seed works one-shot too:

> salmon-ops-serve-fixture config --dir /tmp/play --name web --file index.html | salmon-ops-serve-fixture run up

But the point is the loop. Try (typing, or piping in, one line per command):

> salmon-ops-serve-fixture run serve
> up --dir /tmp/play --name web --file index.html --file style.css
> up --dir /tmp/play --name api --file openapi.json
> status
> down --dir /tmp/play --name web --file index.html --file style.css
> only --dir /tmp/play --name api --file openapi.json --file CHANGELOG
> history
> quit

Notes to notice while playing:

  * the base directory itself is one node shared by every seed (they unify by
    'Salmon.Op.Ref.Ref'), so it survives until the last seed under it is gone;
  * re-declaring an unchanged seed converges to a no-op (nothing is re-run);
  * a seed is identified by the /files it asks for/, so @only@ with a
    different --file set supersedes rather than adds;
  * @status@ shows each node's wanted direction and whether it has converged.

= The @--daemon@ half

Everything above is one-shot nodes: @up@ runs, and what it leaves behind
stays put on its own. @--daemon@ adds the two things that are not that — a
node that /owns a running process/
('Salmon.Builtin.Extension.managed'), and a node whose going away
/bounces what stands on it/ ('Salmon.Op.Supervision.RestForOne') — which
otherwise exist only inside the test suite.

It adds two nodes to the bundle: a @daemon.conf@ under the bundle directory
(the only node here with a @check@ of its own, and the one declaring
@RestForOne@), and a process that reads that file __once at startup__ and
then appends a line to @\<dir\>\/\<name\>.log@ every second. The log is
deliberately outside the bundle directory: nothing declares it, so nothing
removes it, and you can read it across as many up\/down cycles as you like.

Supervision only runs while the loop is __idle__, so none of this shows up
under @serve \< script@ — every line of a piped script is already queued
before the first pass ends. Type at it, or drive it from a fifo.

> salmon-ops-serve-fixture run serve
> up --dir /tmp/play --name web --daemon --greeting hello

then, in another terminal, @tail -f \/tmp\/play\/web.log@ to watch it.

__Bouncing on drift.__ Edit @\/tmp\/play\/web\/daemon.conf@ yourself. The
config node's own machine notices (its @check@ compares the content), rewrites
it, and because it declared @RestForOne@ the process reading it is sent back:

> Signalling "web" 15
> Reaped "web"
> serve: daemon sent back to wait: ... stopped being up
> Spawned "web" (Just ...)

Note the teardown happens /before/ the node is sent back — the process is
stopped through its own bracket first, and only then does the node go looking
for its dependency again.

__Bouncing on a re-declaration.__

> only --dir /tmp/play --name web --daemon --greeting goodbye

The log's next line has both a new pid and the new greeting. Worth watching
the reports for /where/ that happens, because it is not where one would
guess: the convergence pass says @converging (0 down, 0 up)@ and does
nothing, since the node is already 'Salmon.Actions.Serve.Converged' under a
'Salmon.Op.Ref.Ref' that did not change. What applies the new content is the
config node's own machine, on its next look. A node with no @check@ — every
other node in this fixture — would simply keep the old content.

__What a node's own check is and is not allowed to say.__ Add
@--stale-check@ and the daemon node gains a plausible-looking health check:
"my log file exists". It is wrong in the way health checks are wrong — it
answers "this ran at some point", not "it is running now" — and doing either
of the above shows it being __deliberately ignored__: the node is torn down
and put straight back, because the machine that cancelled the action knows
better than any check can.

That is a fix rather than the original behaviour. Until (I1) in
@specs\/per-node-state-machines-remaining.md@ was settled, this flag lost the
process outright: the check was consulted, it said the effect was in place,
and the node settled into 'Salmon.Actions.Upkeep.Up' holding nothing at all.
The flag stays because the rule it demonstrates is worth being able to see.
-}
module Main (main) where

import Control.Exception (throwIO)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import GHC.Generics (Generic)
import Options.Applicative (execParser, fullDesc, header, info, long, many, metavar, progDesc, strOption, switch, value)
import qualified Options.Applicative as Opt
import Options.Generic (ParseRecord (..))
import System.Directory (doesFileExist, removeFile)
import System.FilePath (takeDirectory, (<.>), (</>))
import System.Process (proc)

import Salmon.Actions.UpDown (CheckResult (..))
import qualified Salmon.Builtin.CommandLine as CLI
import Salmon.Builtin.Extension (Op, Track', check, deps, down, dynamics, help, managed, notes, op, ref, up)
import qualified Salmon.Builtin.Nodes.Daemon as Daemon
import qualified Salmon.Builtin.Nodes.Filesystem as FS
import Salmon.Op.Configure (Configure (..))
import Salmon.Op.Ref (mkRef)
import Salmon.Op.Supervision (Strategy (..), Supervision (..), defaultSupervision, supervised)
import Salmon.Op.Track (Track (..))
import Salmon.Reporter (reportIf, reportPrint)

-------------------------------------------------------------------------------

-- | What a human types after @config@ / @up@ / @down@ / @only@.
data Seed = Seed
    { seedDir :: FilePath
    , seedName :: Text
    , seedFiles :: [String]
    , seedDaemon :: Bool
    , seedGreeting :: Text
    , seedStaleCheck :: Bool
    }
    deriving (Generic, Show)

instance ParseRecord Seed where
    parseRecord =
        Seed
            <$> strOption
                (long "dir" <> metavar "DIR" <> value "/tmp/salmon-serve-fixture" <> Opt.help "base directory to converge files under")
            <*> fmap Text.pack (strOption (long "name" <> metavar "NAME" <> Opt.help "name of this bundle (its own subdirectory)"))
            <*> many (strOption (long "file" <> metavar "FILE" <> Opt.help "a file to keep in the bundle (repeatable)"))
            <*> switch (long "daemon" <> Opt.help "also run a process that reads this bundle's config and logs it once a second")
            <*> fmap
                Text.pack
                ( strOption
                    (long "greeting" <> metavar "TEXT" <> value "hello" <> Opt.help "what the daemon's config file says (--daemon only)")
                )
            <*> switch
                ( long "stale-check"
                    <> Opt.help "give the daemon a plausible-but-stale check (\"my log exists\"), which a bounce then ignores. See (I1)."
                )

-- | The hermetic directive: same shape as the seed here, but that's a
-- coincidence of how trivial this fixture is — the point of the type is that
-- it is 'FromJSON'\/'ToJSON', so it is what identifies a seed in the loop.
data Spec = Spec
    { specRoot :: FilePath
    , specFiles :: [FilePath]
    , specDaemon :: Maybe DaemonSpec
    }
    deriving (Eq, Show, Generic)

-- | Everything the @--daemon@ half needs, absent when it was not asked for.
data DaemonSpec = DaemonSpec
    { daemonName :: Text
    , daemonConf :: FilePath
    , daemonLog :: FilePath
    , daemonGreeting :: Text
    , daemonStaleCheck :: Bool
    }
    deriving (Eq, Show, Generic)

instance FromJSON Spec
instance ToJSON Spec
instance FromJSON DaemonSpec
instance ToJSON DaemonSpec

configure :: Configure IO Seed Spec
configure = Configure $ \seed ->
    let root = seed.seedDir </> Text.unpack seed.seedName
     in pure $
            Spec
                root
                [root </> f | f <- seed.seedFiles]
                ( if not seed.seedDaemon
                    then Nothing
                    else
                        Just $
                            DaemonSpec
                                { daemonName = seed.seedName
                                , daemonConf = root </> "daemon.conf"
                                , -- deliberately /outside/ the bundle directory, and
                                  -- so not a node: nothing declares it, nothing
                                  -- removes it, and `dir`'s non-recursive `down`
                                  -- does not trip over it on the way out.
                                  daemonLog = seed.seedDir </> Text.unpack seed.seedName <.> "log"
                                , daemonGreeting = seed.seedGreeting
                                , daemonStaleCheck = seed.seedStaleCheck
                                }
                )

program :: Track' Spec
program = Track $ \spec ->
    op "serve-fixture-bundle" (deps (fmap fileOp spec.specFiles <> foldMap (pure . daemonOp) spec.specDaemon)) $ \actions ->
        actions{ref = mkRef "serve-fixture-bundle" (spec.specRoot, spec.specFiles, fmap daemonConf spec.specDaemon)}
  where
    fileOp :: FilePath -> Op
    fileOp path =
        FS.filecontents (FS.FileContents path (Text.pack ("managed by salmon-ops-serve-fixture: " <> path <> "\n")))

-------------------------------------------------------------------------------
-- the --daemon half: a process salmon owns, and the config it stands on

{- | The daemon's configuration file, and the only node in this fixture with
a @check@ of its own.

That is the whole reason it exists rather than reusing
'Salmon.Builtin.Nodes.Filesystem.filecontents'. @filecontents@ has no
@check@, so it answers @Unknown@ forever, and a node that answers @Unknown@
can never be seen to have stopped being up — which means it can never demote
anything either. Supervision is only as good as the nodes' ability to answer
"is my effect still there", and this is what that answer looks like: read the
file, compare it with what it should say.

'RestForOne' is authored here, on the file, rather than on the daemon that
reads it. Only the file's author knows its content is load-bearing.
-}
configOp :: DaemonSpec -> Op
configOp d =
    op "daemon-config" (deps [FS.dir (FS.Directory (takeDirectory d.daemonConf))]) $ \actions ->
        actions
            { help = Text.pack ("keeps " <> d.daemonConf <> " saying " <> Text.unpack d.daemonGreeting)
            , notes =
                [ "has a check, unlike `filecontents`, so a supervisor can notice it changing"
                , "declares RestForOne, so whatever reads it is bounced when it does"
                ]
            , -- keyed on the path alone, so re-declaring the bundle with a
              -- different --greeting is the /same node/ with different
              -- content rather than a second node.
              ref = mkRef "daemon-config" d.daemonConf
            , check = do
                there <- doesFileExist d.daemonConf
                if not there
                    then pure (Failure "no config file yet")
                    else do
                        actual <- Text.IO.readFile d.daemonConf
                        pure $
                            if actual == body
                                then Success
                                else Failure "the config file no longer says what it should"
            , up = Text.IO.writeFile d.daemonConf body
            , down = removeFile d.daemonConf
            , dynamics = [supervised defaultSupervision{supStrategy = RestForOne}]
            }
  where
    body :: Text
    body = "greeting = " <> d.daemonGreeting <> "\n"

{- | A process salmon owns, which reads the config __once at startup__ and
then logs what it read every second.

Reading once is what makes the demonstration honest: the running process
holds content that can go stale, and nothing but restarting it can make it
notice. Written out rather than going through
'Salmon.Builtin.Nodes.Daemon.daemon' because this node wants two things that
function does not offer — a dependency, and (with @--stale-check@) a @check@
of its own — which is exactly the case @runDaemon@ is exposed for.
-}
daemonOp :: DaemonSpec -> Op
daemonOp d =
    op "daemon" (deps [configOp d]) $ \actions ->
        actions
            { help = Text.pack ("keeps a process logging to " <> d.daemonLog)
            , ref = Daemon.daemonRef spawn
            , managed = Just (Daemon.runDaemon chatter spawn)
            , check =
                if not d.daemonStaleCheck
                    then pure Unknown
                    else do
                        -- A health check somebody might plausibly write, and
                        -- which is wrong in the way health checks are wrong:
                        -- it answers "something ran once", not "it is running
                        -- now". A bounce ignores it, because the machine that
                        -- cancelled the action knows better; see (I1) in
                        -- specs/per-node-state-machines-remaining.md for what
                        -- it used to do instead.
                        there <- doesFileExist d.daemonLog
                        pure (if there then Success else Failure "no log yet")
            , -- `run up` has nowhere to put an action that never returns.
              up = throwIO (Daemon.NeedsSupervisor d.daemonName)
            , -- under `serve` the process died when its machine was
              -- cancelled; under `run down` it never held one.
              down = pure ()
            }
  where
    spawn :: Daemon.Daemon
    spawn =
        Daemon.defaultDaemon
            d.daemonName
            (proc "/bin/sh" ["-c", script])

    script :: String
    script =
        mconcat
            [ "cfg=$(cat "
            , d.daemonConf
            , "); while true; do echo \"[pid $$] $cfg\" >> "
            , d.daemonLog
            , "; sleep 1; done"
            ]

    -- everything the daemon does /except/ repeat its own output back: the
    -- process writes a line a second and the node's ring already has them.
    chatter = reportIf notWrote reportPrint
    notWrote Daemon.Wrote{} = False
    notWrote _ = True

-------------------------------------------------------------------------------

main :: IO ()
main = do
    let desc = fullDesc <> progDesc "play with `run serve` against files in a directory" <> header "salmon-ops-serve-fixture"
    cmd <- execParser (info parseRecord desc)
    CLI.execCommandOrSeed reportPrint configure program cmd
