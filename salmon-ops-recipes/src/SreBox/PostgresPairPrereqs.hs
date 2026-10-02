{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | What "SreBox.PostgresPair" assumes was done before it ran, as nodes.

The pair is handed paths: two @.pgpass@ files on each member, a console
@.pgpass@ and a @userlist.txt@ on each bouncer, and it expects the packages
to be there and the application to have a role, a database and a way in.
This module declares that layer, so that a consumer of the pair does not
write it again -- it is the part where mistakes are quiet (a @userlist.txt@
written under a running pgbouncer is a correct password that does not work).

= Secrets are still somebody else's files

Nothing here carries a password and nothing here decides how one travels.
Every secret is a file that is /already on the machine it is for/, put there
by whatever the deployment uses for that; a 'SecretFile' says where it was
left ('secret_from') and this recipe installs it where the pair reads it,
with the owner and mode the pair needs. A file left at its destination
already ('secret_from' @= Nothing@) only has its owner and mode set.

The scripts contain paths, never contents, and say nothing about a file but
its name: a missing file is named, a password is read on the machine and fed
to @psql@ on standard input, and the one statement that holds it has its
output withheld from the report.

= What it declares

* 'memberPrereqs', per member: the packages, a cluster of the declared name
  to exist, and the replication and rewind passfiles.
* 'bouncerPrereqs', per bouncer: the packages, the console passfile, and the
  auth file -- installed only when it differs, and pgbouncer restarted only
  then, because pgbouncer reads it when it starts and a restart drops the
  clients it is holding.
* 'applications', per member: a @pg_hba.conf@ line per application and
  client (on both members: that file is not replicated), and, on whichever
  member is the primary, the role, its password and its database.

'pairWithPrereqs' is the pair with all of it, in order: a machine's
prerequisites before the pair's own node for that machine, and the
applications after the role node -- once there is exactly one primary to
create a role on. Before it, a member that is about to be cloned is a
pristine cluster not in recovery, and a database created there is what makes
the clone refuse to run over it.
-}
module SreBox.PostgresPairPrereqs (
    -- * Declaring them
    SecretFile (..),
    postgresOwned,
    inPlace,
    Application (..),
    Prereqs (..),
    defaultPrereqs,
    validate,

    -- * The nodes
    memberPrereqs,
    bouncerPrereqs,
    applications,
    pairWithPrereqs,

    -- * The scripts they run
    memberPrereqsScript,
    bouncerPrereqsScript,
    applicationsScript,
    hbaLines,
) where

import Control.Exception (throwIO)
import Control.Monad (unless)
import Data.Aeson (FromJSON, ToJSON)
import Data.Char (isAsciiLower, isDigit, isOctDigit)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, (</>))

import Salmon.Builtin.Extension
import Salmon.Op.OpGraph (inject)
import Salmon.Op.Ref (mkRef)
import Salmon.Reporter

import SreBox.PostgresPair (Bouncer (..), Member (..), Pair (..), Side (..), Target (..), memberOn)
import qualified SreBox.PostgresPair as Pair

-------------------------------------------------------------------------------
-- Declaring them

{- | A secret somebody else put on the machine, and how it should be held
once it is where the pair reads it. The destination is not here: the pair
already names it.
-}
data SecretFile
    = SecretFile
    { secret_from :: Maybe FilePath
    -- ^ where it was provisioned, on the machine it is for. 'Nothing' (or
    -- the destination itself) is a file left in place: only its owner and
    -- mode are set.
    , secret_owner :: Text
    , secret_group :: Text
    , secret_mode :: Text
    -- ^ octal, as @install -m@ takes it.
    }
    deriving (Eq, Show, Generic)

instance FromJSON SecretFile
instance ToJSON SecretFile

{- | @postgres:postgres@, @0600@: what a passfile must be, since libpq
ignores one that anybody else can read and the pair's commands run as
@postgres@.
-}
postgresOwned :: FilePath -> SecretFile
postgresOwned from = SecretFile (Just from) "postgres" "postgres" "0600"

-- | The same, for a file provisioned straight to where the pair reads it.
inPlace :: SecretFile
inPlace = SecretFile Nothing "postgres" "postgres" "0600"

{- | One application's way into the pair: a role, the database it owns, and
who may connect as it.

The pair knows nothing of this -- it routes a database, it does not own one
-- which is why it is declared here and not there.
-}
data Application
    = Application
    { app_role :: Text
    , app_database :: Text
    , app_passfile :: FilePath
    -- ^ the role's password, in @.pgpass@ format, on /each member/ (either
    -- may be the primary). Read where it lies, by the login the controller
    -- uses; the password is the fifth field of its first line, as for the
    -- pair's own passfiles.
    , app_clients :: [Text]
    -- ^ the addresses that connect as this role -- the bouncers, as the
    -- members see them, which is not necessarily 'bouncer_ssh_host'. A bare
    -- address is one host; anything with a @\/@ is taken as written.
    , app_hba_method :: Text
    -- ^ @md5@ (which also accepts a SCRAM-stored password) or
    -- @scram-sha-256@.
    }
    deriving (Eq, Show, Generic)

instance FromJSON Application
instance ToJSON Application

data Prereqs
    = Prereqs
    { prereq_member_packages :: [Text]
    , prereq_bouncer_packages :: [Text]
    -- ^ Debian packages, installed only if missing. Empty for machines
    -- whose packages are somebody else's (an image, another recipe).
    , prereq_repl_passfile :: SecretFile
    -- ^ becomes 'pair_repl_passfile', on both members.
    , prereq_rewind_passfile :: SecretFile
    -- ^ becomes 'pair_rewind_passfile', on both members.
    , prereq_console_passfile :: SecretFile
    -- ^ becomes 'bouncer_console_passfile', on every bouncer.
    , prereq_userlist :: SecretFile
    -- ^ becomes @userlist.txt@ in 'bouncer_config_dir', on every bouncer: a
    -- file already in pgbouncer's @auth_file@ format, whatever hashing its
    -- author chose. Left in place ('secret_from' @= Nothing@) nothing can
    -- tell that it changed, so nothing restarts pgbouncer: whoever writes
    -- it there owns that.
    , prereq_applications :: [Application]
    }
    deriving (Eq, Show, Generic)

instance FromJSON Prereqs
instance ToJSON Prereqs

{- | Debian's packages, every secret left in place, and no application: the
smallest thing to amend.
-}
defaultPrereqs :: Prereqs
defaultPrereqs =
    Prereqs
        { prereq_member_packages = ["postgresql", "sudo"]
        , prereq_bouncer_packages = ["pgbouncer", "postgresql-client"]
        , prereq_repl_passfile = inPlace
        , prereq_rewind_passfile = inPlace
        , prereq_console_passfile = inPlace
        , -- pgbouncer runs as postgres on Debian, and nobody else needs it
          prereq_userlist = SecretFile Nothing "postgres" "postgres" "0640"
        , prereq_applications = []
        }

{- | Everything wrong with a declaration, not the first thing.

These values are spliced into shell scripts and SQL run as root on three
machines, so what is checked is that each is the kind of word it claims to
be. A node whose declaration does not pass refuses to run and says all of
this; a binary can ask earlier, where the directive is built.
-}
validate :: Pair -> Prereqs -> [Text]
validate pair pre =
    concat
        [ concatMap (package "member") pre.prereq_member_packages
        , concatMap (package "bouncer") pre.prereq_bouncer_packages
        , secret "replication passfile" pre.prereq_repl_passfile
        , secret "rewind passfile" pre.prereq_rewind_passfile
        , secret "console passfile" pre.prereq_console_passfile
        , secret "userlist" pre.prereq_userlist
        , concatMap application pre.prereq_applications
        ]
  where
    package whose p =
        [ whose <> " package " <> quoted p <> " is not a Debian package name"
        | not (packageName p)
        ]
    secret :: Text -> SecretFile -> [Text]
    secret what s =
        concat
            [ [what <> ": " <> quoted (Text.pack from) <> " is not an absolute path" | Just from <- [s.secret_from], not (plainPath from)]
            , [what <> ": owner " <> quoted s.secret_owner <> " is not a user name" | not (accountName s.secret_owner)]
            , [what <> ": group " <> quoted s.secret_group <> " is not a group name" | not (accountName s.secret_group)]
            , [what <> ": mode " <> quoted s.secret_mode <> " is not an octal mode" | not (octalMode s.secret_mode)]
            ]
    application :: Application -> [Text]
    application app =
        concat
            [ [who <> ": role " <> quoted app.app_role <> " is not a plain identifier" | not (identifier app.app_role)]
            , [who <> ": database " <> quoted app.app_database <> " is not a plain identifier" | not (identifier app.app_database)]
            , [ who <> ": role " <> quoted app.app_role <> " is the pair's own, whose password and attributes the pair sets"
              | app.app_role `elem` [pair.pair_repl_role, pair.pair_rewind_role]
              ]
            , [who <> ": passfile " <> quoted (Text.pack app.app_passfile) <> " is not an absolute path" | not (plainPath app.app_passfile)]
            , [who <> ": client " <> quoted c <> " is not an address" | c <- app.app_clients, not (address c)]
            , [ who <> ": " <> quoted app.app_hba_method <> " is not md5 or scram-sha-256"
              | app.app_hba_method `notElem` ["md5", "scram-sha-256"]
              ]
            ]
      where
        who = "application " <> quoted app.app_role

    quoted t = "\"" <> t <> "\""

    identifier t =
        not (Text.null t)
            && Text.length t <= 63
            && Text.all (\c -> isAsciiLower c || isDigit c || c == '_') t
            && not (isDigit (Text.head t))
    accountName t =
        not (Text.null t)
            && Text.all (\c -> isAsciiLower c || isDigit c || c `elem` ("_-" :: String)) t
            && not (Text.head t == '-')
    packageName t =
        not (Text.null t)
            && Text.all (\c -> isAsciiLower c || isDigit c || c `elem` ("+.-" :: String)) t
            && (isAsciiLower (Text.head t) || isDigit (Text.head t))
    octalMode t = Text.length t `elem` [3, 4] && Text.all isOctDigit t
    plainPath p = take 1 p == "/" && all (`notElem` ("\n\r" :: String)) p
    address t =
        not (Text.null t)
            && Text.all (\c -> isAsciiLower c || isDigit c || c `elem` ("ABCDEF.:/" :: String)) t

-------------------------------------------------------------------------------
-- The scripts

{- | The script 'memberPrereqs' runs: packages, the cluster the pair names,
and the two passfiles.
-}
memberPrereqsScript :: Pair -> Prereqs -> Side -> String
memberPrereqsScript pair pre side =
    unlines $
        ["set -e"]
            <> packagesFragment pre.prereq_member_packages
            <> [ -- the pair's every script starts by finding this cluster's
                 -- version, and an empty answer there reads as a path with a
                 -- hole in it; here it reads as what it is.
                 "pg_lsclusters --no-header | awk -v c=" <> shQuote cluster <> " '$2==c' | grep -q . || { echo "
                    <> shQuote ("no Postgres cluster named " <> cluster <> " on this machine")
                    <> " >&2; exit 1; }"
               ]
            <> installFragment pre.prereq_repl_passfile pair.pair_repl_passfile
            <> installFragment pre.prereq_rewind_passfile pair.pair_rewind_passfile
  where
    cluster = Text.unpack (memberOn pair side).member_cluster

{- | The script 'bouncerPrereqs' runs: packages, the console passfile, and
the auth file.

pgbouncer reads its auth file when it starts and not again, so a userlist
written under a running process is a password that does not work yet -- and
the symptom is an authentication failure with a correct password in a
correct file. Hence the restart; and hence only when the file changed,
because on every later pass a restart would drop the very clients the
bouncer is there to hold. @try-restart@, so that a bouncer that is not
running yet is left for the pair's own node to start, on its own ini.
-}
bouncerPrereqsScript :: Prereqs -> Bouncer -> String
bouncerPrereqsScript pre b =
    unlines $
        ["set -e"]
            <> packagesFragment pre.prereq_bouncer_packages
            <> installFragment pre.prereq_console_passfile b.bouncer_console_passfile
            <> ["salmon_changed=0"]
            <> installFragment pre.prereq_userlist (b.bouncer_config_dir </> "userlist.txt")
            <> [ "if [ \"$salmon_changed\" = 1 ]; then systemctl try-restart pgbouncer; fi"
               ]

{- | Installs one pre-provisioned file where the pair reads it.

Compared before it is copied, so an unchanged file is not rewritten (and
@salmon_changed@, which the auth file's restart reads, stays as it was);
owner and mode are set either way, since they are sets rather than inserts.
What it says on failure is the file's /name/.
-}
installFragment :: SecretFile -> FilePath -> [String]
installFragment s dest =
    case s.secret_from of
        Just from
            | from /= dest ->
                [ "[ -r " <> shQuote from <> " ] || { echo " <> shQuote ("pre-provisioned file missing or unreadable: " <> from) <> " >&2; exit 1; }"
                , "mkdir -p " <> shQuote (takeDirectory dest)
                , "if ! cmp -s " <> shQuote from <> " " <> shQuote dest <> "; then"
                , "  install -m " <> mode <> " -o " <> owner <> " -g " <> group <> " " <> shQuote from <> " " <> shQuote dest
                , "  salmon_changed=1"
                , "fi"
                ]
                    <> hold
        _ ->
            ["[ -e " <> shQuote dest <> " ] || { echo " <> shQuote ("pre-provisioned file missing: " <> dest) <> " >&2; exit 1; }"]
                <> hold
  where
    owner = shQuote (Text.unpack s.secret_owner)
    group = shQuote (Text.unpack s.secret_group)
    mode = shQuote (Text.unpack s.secret_mode)
    hold =
        [ "chown " <> shQuote (Text.unpack (s.secret_owner <> ":" <> s.secret_group)) <> " " <> shQuote dest
        , "chmod " <> mode <> " " <> shQuote dest
        ]

{- | Installs what is missing and nothing else: a machine whose packages are
all there -- baked into an image, say, with no route to a mirror -- never
reaches @apt-get@.
-}
packagesFragment :: [Text] -> [String]
packagesFragment [] = []
packagesFragment packages =
    [ "salmon_missing=''"
    , "for p in " <> unwords (map (shQuote . Text.unpack) packages) <> "; do"
    , "  dpkg-query -W -f='${Status}' \"$p\" 2>/dev/null | grep -q 'install ok installed' || salmon_missing=\"$salmon_missing $p\""
    , "done"
    , "if [ -n \"$salmon_missing\" ]; then DEBIAN_FRONTEND=noninteractive apt-get install -y $salmon_missing; fi"
    ]

-- | The @pg_hba.conf@ lines the applications need, on either member.
hbaLines :: Prereqs -> [Text]
hbaLines pre =
    [ Text.unwords ["host", app.app_database, app.app_role, cidr client, app.app_hba_method]
    | app <- pre.prereq_applications
    , client <- app.app_clients
    ]
  where
    cidr c
        | "/" `Text.isInfixOf` c = c
        | ":" `Text.isInfixOf` c = c <> "/128"
        | otherwise = c <> "/32"

{- | The script 'applications' runs on a member.

The @pg_hba.conf@ lines go on whichever half this is, because that file
lives outside the data directory and reaches the other machine through
nothing. Roles and databases are catalog rows and do reach it, through the
WAL, so only a primary makes them -- the same rule as the pair's own roles.

The password is read here, from the file the declaration names, with its
quotes doubled, and reaches @psql@ on standard input: never an argument,
which is in @ps@. That one statement's output is withheld, because psql
quotes the line it failed on and this is the line with the password in it;
the failure is reported in words instead.
-}
applicationsScript :: Pair -> Prereqs -> Side -> String
applicationsScript pair pre side =
    unlines $
        [ "set -e"
        , "version=$(pg_lsclusters --no-header | awk -v c=" <> shQuote cluster <> " '$2==c {print $1}' | head -n1)"
        , "[ -n \"$version\" ] || { echo " <> shQuote ("no Postgres cluster named " <> cluster <> " on this machine") <> " >&2; exit 1; }"
        , "hba=/etc/postgresql/$version/" <> cluster <> "/pg_hba.conf"
        ]
            <> map hbaLine (hbaLines pre)
            <> [ psql "SELECT pg_reload_conf()" <> " >/dev/null"
               , "if [ \"$(" <> psql "SELECT pg_is_in_recovery()" <> ")\" = f ]; then"
               ]
            <> concatMap (map ("  " <>) . application) pre.prereq_applications
            <> [ "  :"
               , "fi"
               ]
  where
    m = memberOn pair side
    cluster = Text.unpack m.member_cluster
    port = show m.member_port

    hbaLine l = "grep -qxF " <> shQuote (Text.unpack l) <> " \"$hba\" || echo " <> shQuote (Text.unpack l) <> " >> \"$hba\""

    psqlBase = "sudo -u postgres psql -p " <> port <> " -tAX -v ON_ERROR_STOP=1 -d postgres"
    psql sql = psqlBase <> " -c " <> shQuote sql

    application :: Application -> [String]
    application app =
        [ "[ -r " <> shQuote app.app_passfile <> " ] || { echo " <> shQuote ("pre-provisioned file missing or unreadable: " <> app.app_passfile) <> " >&2; exit 1; }"
        , "salmon_pw=$(awk -F: 'NR==1 {print $5}' " <> shQuote app.app_passfile <> " | sed \"s/'/''/g\")"
        , "[ -n \"$salmon_pw\" ] || { echo " <> shQuote ("no password on the first line of " <> app.app_passfile) <> " >&2; exit 1; }"
        , psql ("DO $do$ BEGIN CREATE ROLE " <> role <> " LOGIN; EXCEPTION WHEN duplicate_object THEN NULL; END $do$") <> " >/dev/null"
        , -- created if missing and its password set either way, so that
          -- rotating the passfile is enough to rotate the role. The heredoc's
          -- delimiter is unquoted on purpose: the shell substitutes the
          -- password it just read.
          psqlBase
            <> " >/dev/null 2>&1 <<SALMON_APP_SQL || { echo "
            <> shQuote ("could not set the password of role " <> role <> " (psql's output is withheld: it quotes the statement)")
            <> " >&2; exit 1; }\nALTER ROLE "
            <> role
            <> " LOGIN PASSWORD '$salmon_pw';\nSALMON_APP_SQL"
        , "unset salmon_pw"
        , -- CREATE DATABASE runs in no transaction and has no IF NOT EXISTS
          psql ("SELECT 1 FROM pg_database WHERE datname = '" <> db <> "'")
            <> " | grep -q 1 || "
            <> psql ("CREATE DATABASE " <> db <> " OWNER " <> role)
            <> " >/dev/null"
        ]
      where
        role = Text.unpack app.app_role
        db = Text.unpack app.app_database

shQuote :: String -> String
shQuote s = "'" <> concatMap (\c -> if c == '\'' then "'\\''" else [c]) s <> "'"

-------------------------------------------------------------------------------
-- The nodes

{- | What a machine needs before it can be a member: the packages, a cluster
of the declared name, and the two passfiles where the pair reads them.

Like the pair's own member node it says nothing about which half this is,
and it has no @check@: everything it does is compare-then-set, and asking a
machine whether all of it holds costs the round trip that doing it does.
-}
memberPrereqs :: Reporter Pair.Report -> Pair -> Prereqs -> Side -> Op
memberPrereqs r pair pre side =
    op "pg-pair-member-prereqs" nodeps $ \actions ->
        actions
            { ref = mkRef "pg-pair-member-prereqs" (pair.pair_name <> "@" <> m.member_host)
            , help = Text.unwords ["what", m.member_host, "needs to be a member of", pair.pair_name]
            , notes =
                [ "packages: " <> packagesNote pre.prereq_member_packages
                , secretNote "replication passfile" pre.prereq_repl_passfile pair.pair_repl_passfile
                , secretNote "rewind passfile" pre.prereq_rewind_passfile pair.pair_rewind_passfile
                ]
            , up = runOn r pair pre (OnMember m) m.member_host (memberPrereqsScript pair pre side)
            }
  where
    m = memberOn pair side

{- | What a machine needs before it can be this pair's bouncer: the
packages, the console passfile, and the auth file -- see
'bouncerPrereqsScript' for when pgbouncer is restarted and when not.
-}
bouncerPrereqs :: Reporter Pair.Report -> Pair -> Prereqs -> Bouncer -> Op
bouncerPrereqs r pair pre b =
    op "pg-pair-bouncer-prereqs" nodeps $ \actions ->
        actions
            { ref = mkRef "pg-pair-bouncer-prereqs" (pair.pair_name <> "@" <> b.bouncer_ssh_host)
            , help = Text.unwords ["what", b.bouncer_name, "needs to be a bouncer of", pair.pair_name]
            , notes =
                [ "packages: " <> packagesNote pre.prereq_bouncer_packages
                , secretNote "console passfile" pre.prereq_console_passfile b.bouncer_console_passfile
                , secretNote "auth file" pre.prereq_userlist (b.bouncer_config_dir </> "userlist.txt")
                , "pgbouncer is restarted only when the auth file changed"
                ]
            , up = runOn r pair pre (OnBouncer b) b.bouncer_name (bouncerPrereqsScript pre b)
            }

{- | The applications' roles, databases and @pg_hba.conf@ lines, on one
member. Declared for both: the lines are per machine, and the rest is done
by whichever is the primary when the node runs.
-}
applications :: Reporter Pair.Report -> Pair -> Prereqs -> Side -> Op
applications r pair pre side =
    op "pg-pair-applications" nodeps $ \actions ->
        actions
            { ref = mkRef "pg-pair-applications" (pair.pair_name <> "@" <> m.member_host)
            , help = Text.unwords ["applications of", pair.pair_name, "on", m.member_host]
            , notes =
                [ Text.unwords
                    [ "role"
                    , app.app_role
                    , "owns database"
                    , app.app_database <> ","
                    , "password from"
                    , Text.pack app.app_passfile <> ","
                    , "reached from"
                    , if null app.app_clients then "nowhere" else Text.intercalate ", " app.app_clients
                    ]
                | app <- pre.prereq_applications
                ]
            , up = runOn r pair pre (OnMember m) m.member_host (applicationsScript pair pre side)
            }
  where
    m = memberOn pair side

packagesNote :: [Text] -> Text
packagesNote [] = "none (somebody else's)"
packagesNote ps = Text.unwords ps

-- | Paths, owner and mode: what a reader of @run tree@ may know of a secret.
secretNote :: Text -> SecretFile -> FilePath -> Text
secretNote what s dest =
    Text.unwords
        [ what
        , Text.pack dest
        , s.secret_owner <> ":" <> s.secret_group
        , s.secret_mode <> ","
        , case s.secret_from of
            Just from | from /= dest -> "installed from " <> Text.pack from
            _ -> "provisioned in place"
        ]

runOn :: Reporter Pair.Report -> Pair -> Prereqs -> Target -> Text -> String -> IO ()
runOn r pair pre target name script = do
    let refusals = validate pair pre
    unless (null refusals) $
        throwIO (userError (Text.unpack ("prerequisites of " <> pair.pair_name <> ": " <> Text.intercalate "; " refusals)))
    (code, out, err) <- Pair.sshToTarget pair target script
    runReporter r (Pair.Acted name code (Text.strip (out <> err)))
    case code of
        ExitSuccess -> pure ()
        ExitFailure _ ->
            throwIO (userError (Text.unpack ("prerequisites on " <> name <> ": " <> Text.strip (err <> out))))

{- | The pair, and everything it assumed.

The edges are the point, and there are three kinds. A machine's
prerequisites come before the pair's own node for that machine -- declared
by naming that node again with one more dependency, which the walk merges
with the pair's own declaration of it, since they are the same node. The
applications come after 'Pair.pairOp', so after the role node. And nothing
else is ordered: one member's packages do not wait for the other's.
-}
pairWithPrereqs :: Reporter Pair.Report -> Pair -> Prereqs -> Op
pairWithPrereqs r pair pre =
    op "pg-pair-with-prereqs" (deps (apps <> [whole] <> machines)) $ \actions ->
        actions
            { ref = mkRef "pg-pair-with-prereqs" pair.pair_name
            , help = Text.unwords ["the pair", pair.pair_name, "and what it needs first"]
            }
  where
    whole = Pair.pairOp r pair
    machines =
        [Pair.member r pair side `inject` memberPrereqs r pair pre side | side <- [A, B]]
            <> [Pair.bouncerSetup r pair b `inject` bouncerPrereqs r pair pre b | b <- pair.pair_bouncers]
    apps
        | null pre.prereq_applications = []
        | otherwise = [applications r pair pre side `inject` whole | side <- [A, B]]
