module SreBox.Initialize where

import Data.Text (Text)
import qualified Data.Text as Text

import Salmon.Builtin.Extension
import qualified Salmon.Builtin.Nodes.Debian.OS as Debian
import qualified Salmon.Builtin.Nodes.Filesystem as FS
import qualified Salmon.Builtin.Nodes.User as User
import Salmon.Op.OpGraph (inject)
import Salmon.Op.Track
import Salmon.Reporter

-- | @authorizedKey@, if provided, is the literal contents of a public key
-- (e.g. an @id_ed25519.pub@ line) to allow to log in as the salmon user.
-- Reading it from a file is the caller's job (typically the seed\/Configure
-- step, on the commanding machine) -- this recipe only ever sees key material
-- it's handed, matching every other recipe's key-exchange-agnostic stance.
initialize :: Reporter User.Report -> Maybe Text -> Op
initialize r authorizedKey =
    op "initialize" (deps [sudoerfile, tmpdir, passwordlessUser, homeOwnership]) id
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
    sudoUser = User.user r Debian.useradd mkGroup (User.NewUser user [sudo])

    passwordlessUser :: Op
    passwordlessUser = User.passwordless r Debian.usermod mkUser user

    -- chowned recursively and last, so it also covers .ssh/authorized_keys
    -- if 'authorizedKeyOp' created it.
    homeOwnership :: Op
    homeOwnership =
        maybe id (flip inject) authorizedKeyOp $
            User.chown r Debian.chown True (User.Owner user salmonGroup) "/home/salmon" `inject` tmpdir

    -- forced to run after 'homeDir', rather than relying on `mkdir -p`
    -- implicitly creating /home/salmon as a side effect of some other subdir
    authorizedKeyOp :: Maybe Op
    authorizedKeyOp =
        (`inject` homeDir) . FS.appendLineIfMissing . FS.AppendLineIfMissing "/home/salmon/.ssh/authorized_keys" <$> authorizedKey

    salmonGroup :: User.Group
    salmonGroup = User.Group "salmon"

    mkUser :: Track' User.User
    mkUser = Track $ \u -> User.user r Debian.useradd mkGroup (User.NewUser u [sudo])

    mkGroup :: Track' User.Group
    mkGroup = Track $ \grp -> User.group r Debian.groupadd grp

    user :: User.User
    user = User.User "salmon"

    sudo :: User.Group
    sudo = User.Group "sudo"

    -- /home/salmon/tmp and /home/salmon/.ssh both live under the user's home
    -- dir; force that ordering explicitly with 'inject' rather than relying
    -- on `mkdir -p`'s incidental parent-creation, so the graph reflects the
    -- real "home dir first" relationship (relevant once this recipe is
    -- generalized to initializing more than the single hardcoded salmon user).
    homeDir :: Op
    homeDir = FS.dir (FS.Directory "/home/salmon") `inject` sudoUser

    tmpdir :: Op
    tmpdir = FS.dir (FS.Directory "/home/salmon/tmp") `inject` homeDir
