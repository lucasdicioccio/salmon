module Salmon.Builtin.Nodes.User where

import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Binary
import Salmon.Builtin.Nodes.Filesystem
import Salmon.Op.Ref
import Salmon.Op.Track

import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as Text

import System.FilePath ((</>))
import System.Process.ByteString (readCreateProcessWithExitCode)
import System.Process.ListLike (CreateProcess (..), proc)

-------------------------------------------------------------------------------

newtype Group = Group {groupName :: Text}

group :: Track' (Binary "groupadd") -> Group -> Op
group groupadd grp =
    withBinary groupadd runGroupAdd (AddGroup grp.groupName) $ \add ->
        op "group" nodeps $ \actions ->
            actions
                { help = "creates a system group"
                , ref = dotRef $ "group:" <> groupName grp
                , up = add
                }

data GroupAddCommand
    = AddGroup Text

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

user :: Track' (Binary "useradd") -> Track' Group -> NewUser -> Op
user useradd grp nu =
    withBinary useradd runUserAdd (AddUser nu.newUser.userName (fmap groupName nu.groups)) $ \add ->
        op "user" (deps userGroups) $ \actions ->
            actions
                { help = "creates a system user"
                , ref = dotRef $ "user" <> nu.newUser.userName
                , up = add
                }
  where
    userGroups :: [Op]
    userGroups = groupForUser : fmap (run grp) (groups nu)

    groupForUser :: Op
    groupForUser = run grp (Group nu.newUser.userName)

data UserAddCommand
    = AddUser Text [Text]

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
