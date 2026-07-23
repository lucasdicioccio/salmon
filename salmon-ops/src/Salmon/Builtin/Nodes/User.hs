module Salmon.Builtin.Nodes.User where

import Salmon.Actions.UpDown (Requirement (..))
import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Binary (Binary, Command (..), withBinary)
import qualified Salmon.Builtin.Nodes.Binary as Binary
import Salmon.Builtin.Nodes.Filesystem
import Salmon.Op.Ref
import Salmon.Op.Track
import Salmon.Reporter

import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as Text

import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Process.ByteString (readCreateProcessWithExitCode)
import System.Process.ListLike (CreateProcess (..), proc)

-------------------------------------------------------------------------------
data Report
    = RunGroupAdd !GroupAddCommand !Binary.Report
    | RunUserAdd !UserAddCommand !Binary.Report
    | RunUserMod !UserModCommand !Binary.Report
    | RunChown !ChownCommand !Binary.Report
    deriving (Show)

-------------------------------------------------------------------------------

newtype Group = Group {groupName :: Text}

group :: Reporter Report -> Track' (Binary "groupadd") -> Group -> Op
group r groupadd grp =
    withBinary groupadd runGroupAdd cmd $ \add ->
        op "group" nodeps $ \actions ->
            actions
                { help = "creates a system group"
                , ref = mkRef "group" (groupName grp)
                , prelim = skipIfGroupExists grp
                , up = add r'
                }
  where
    cmd = AddGroup grp.groupName
    r' = contramap (RunGroupAdd cmd) r

data GroupAddCommand
    = AddGroup Text
    deriving (Show)

runGroupAdd :: Command "groupadd" GroupAddCommand
runGroupAdd = Command go
  where
    go (AddGroup name) =
        proc
            "groupadd"
            [ Text.unpack name
            ]

-- | @groupadd@ has no idempotent form (no @-f@-equivalent that's safe across
-- all cases), so skip it via 'prelim' if @getent group@ already knows about it.
skipIfGroupExists :: Group -> IO Requirement
skipIfGroupExists grp = do
    (code, _out, _err) <-
        readCreateProcessWithExitCode
            (proc "getent" ["group", Text.unpack grp.groupName])
            ""
    pure $ case code of
        ExitSuccess -> Skippable
        _ -> Required

-------------------------------------------------------------------------------

newtype User = User {userName :: Text}

data NewUser = NewUser {newUser :: User, groups :: [Group]}

user :: Reporter Report -> Track' (Binary "useradd") -> Track' Group -> NewUser -> Op
user r useradd grp nu =
    withBinary useradd runUserAdd cmd $ \add ->
        op "user" (deps userGroups) $ \actions ->
            actions
                { help = "creates a system user"
                , ref = mkRef "user" nu.newUser.userName
                , prelim = skipIfUserExists nu.newUser
                , up = add r'
                }
  where
    cmd = AddUser nu.newUser.userName (fmap groupName nu.groups)
    r' = contramap (RunUserAdd cmd) r

    userGroups :: [Op]
    userGroups = groupForUser : fmap (run grp) (groups nu)

    groupForUser :: Op
    groupForUser = run grp (Group nu.newUser.userName)

data UserAddCommand
    = AddUser Text [Text]
    deriving (Show)

runUserAdd :: Command "useradd" UserAddCommand
runUserAdd = Command go
  where
    go (AddUser name []) =
        proc
            "useradd"
            [ "-M"
            , "-c"
            , "salmon-created user"
            , "-g"
            , Text.unpack name
            , Text.unpack name
            ]
    go (AddUser name grps) =
        proc
            "useradd"
            [ "-M"
            , "-c"
            , "salmon-created user"
            , "-G"
            , Text.unpack $ Text.intercalate "," grps
            , "-g"
            , Text.unpack name
            , Text.unpack name
            ]

-- | @useradd@ has no idempotent form either, so skip it via 'prelim' if
-- @getent passwd@ already knows about it.
skipIfUserExists :: User -> IO Requirement
skipIfUserExists u = do
    (code, _out, _err) <-
        readCreateProcessWithExitCode
            (proc "getent" ["passwd", Text.unpack u.userName])
            ""
    pure $ case code of
        ExitSuccess -> Skippable
        _ -> Required

-------------------------------------------------------------------------------

-- | Clears a user's password (@usermod -p '*'@), locking out password-based
-- login while leaving the account otherwise usable (e.g. for key-based ssh).
-- @usermod -p@ is a set rather than an add, so it's already idempotent and
-- needs no 'prelim' guard.
passwordless :: Reporter Report -> Track' (Binary "usermod") -> Track' User -> User -> Op
passwordless r usermod trackUser u =
    withBinary usermod runUserMod cmd $ \remove ->
        op "passwordless" (deps [run trackUser u]) $ \actions ->
            actions
                { help = "removes a system user's password"
                , ref = mkRef "passwordless" u.userName
                , up = remove r'
                }
  where
    cmd = RemovePassword u.userName
    r' = contramap (RunUserMod cmd) r

data UserModCommand
    = RemovePassword Text
    deriving (Show)

runUserMod :: Command "usermod" UserModCommand
runUserMod = Command go
  where
    go (RemovePassword name) =
        proc
            "usermod"
            [ "-p"
            , "*"
            , Text.unpack name
            ]

-------------------------------------------------------------------------------

-- | A user/group pair to hand to 'chown'.
data Owner = Owner {ownerUser :: User, ownerGroup :: Group}

{- | Sets a path's ownership (@chown user:group path@, optionally @-R@).
@chown@ is a set rather than an add, so it's already idempotent and needs no
'prelim' guard. Unlike 'dir'/'filecontents', this does not itself create the
path — callers are expected to wire the path's own creation as a dependency
(e.g. via 'Salmon.Op.OpGraph.inject').
-}
chown :: Reporter Report -> Track' (Binary "chown") -> Bool -> Owner -> FilePath -> Op
chown r chownBin recursive owner path =
    withBinary chownBin runChown cmd $ \apply ->
        op "chown" nodeps $ \actions ->
            actions
                { help = Text.pack $ "sets ownership of " <> path <> " to " <> Text.unpack ownerText
                , ref = mkRef "chown" (path, owner.ownerUser.userName, owner.ownerGroup.groupName)
                , up = apply r'
                }
  where
    ownerText = owner.ownerUser.userName <> ":" <> owner.ownerGroup.groupName
    cmd = Chown recursive owner.ownerUser.userName owner.ownerGroup.groupName path
    r' = contramap (RunChown cmd) r

data ChownCommand
    = Chown Bool Text Text FilePath
    deriving (Show)

runChown :: Command "chown" ChownCommand
runChown = Command go
  where
    go (Chown recursive u g path) =
        proc
            "chown"
            ( ["-R" | recursive]
                <> [ Text.unpack u <> ":" <> Text.unpack g
                   , path
                   ]
            )
