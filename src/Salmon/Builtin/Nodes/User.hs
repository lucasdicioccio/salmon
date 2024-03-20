module Salmon.Builtin.Nodes.User where

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

import System.FilePath ((</>))
import System.Process.ByteString (readCreateProcessWithExitCode)
import System.Process.ListLike (CreateProcess (..), proc)

-------------------------------------------------------------------------------
data Report
    = RunGroupAdd !GroupAddCommand !Binary.Report
    | RunUserAdd !UserAddCommand !Binary.Report
    deriving (Show)

-------------------------------------------------------------------------------

newtype Group = Group {groupName :: Text}

group :: Reporter Report -> Track' (Binary "groupadd") -> Group -> Op
group r groupadd grp =
    withBinary r' groupadd runGroupAdd cmd $ \add ->
        op "group" nodeps $ \actions ->
            actions
                { help = "creates a system group"
                , ref = dotRef $ "group:" <> groupName grp
                , up = add
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

-------------------------------------------------------------------------------

newtype User = User {userName :: Text}

data NewUser = NewUser {newUser :: User, groups :: [Group]}

user :: Reporter Report -> Track' (Binary "useradd") -> Track' Group -> NewUser -> Op
user r useradd grp nu =
    withBinary r' useradd runUserAdd cmd $ \add ->
        op "user" (deps userGroups) $ \actions ->
            actions
                { help = "creates a system user"
                , ref = dotRef $ "user" <> nu.newUser.userName
                , up = add
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
