module SreBox.Initialize where

import qualified Data.Text as Text
import Data.Text (Text)

import Salmon.Builtin.Extension
import Salmon.Op.OpGraph (inject)
import Salmon.Op.Track
import qualified Salmon.Builtin.Nodes.Debian.OS as Debian
import qualified Salmon.Builtin.Nodes.Filesystem as FS
import qualified Salmon.Builtin.Nodes.User as User

initialize :: Op
initialize =
    op "initialize" (deps [sudoerfile, tmpdir `inject` sudoUser]) id
  where
    sudoerfile :: Op
    sudoerfile =
      FS.filecontents
        (FS.FileContents "/etc/sudoers.d/salmon" sudoercontent)

    sudoercontent :: Text
    sudoercontent =
      Text.unlines
      [ Text.unwords
        [ "salmon"
        , "ALL=(ALL)"
        , "NOPASSWD:"
        , "ALL"
        ]
      ]

    sudoUser :: Op
    sudoUser = User.user Debian.useradd mkGroup (User.NewUser user [sudo])

    mkGroup :: Track' User.Group
    mkGroup = Track $ \grp -> User.group Debian.groupadd grp

    user :: User.User
    user = User.User "salmon"

    sudo :: User.Group
    sudo = User.Group "sudo"

    tmpdir :: Op
    tmpdir = FS.dir (FS.Directory "/home/salmon/tmp")
