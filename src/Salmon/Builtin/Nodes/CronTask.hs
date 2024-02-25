module Salmon.Builtin.Nodes.CronTask where

import Salmon.Builtin.Extension
import Salmon.Op.Ref
import Salmon.Op.Track
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.ByteString as ByteString
import System.FilePath ((</>))
import System.Directory (removeFile)

type Minute = Text
type Hour = Text
type DOM = Text
type Month = Text
type DOW = Text

data Schedule
  = Schedule
  { minute :: Minute
  , hour :: Hour
  , dayOfMonth :: DOM
  , month :: Month
  , dayOfWeek :: DOW
  }

everyMinute :: Schedule
everyMinute = Schedule "*" "*" "*" "*" "*"

data CronTask
  = CronTask
  { name :: Text
  , user :: Text
  , schedule :: Schedule
  , command :: FilePath
  , commandArgs :: [Text]
  }

platformCronPath :: FilePath
platformCronPath = "/etc/cron.d"

crontask :: Track' CronTask -> CronTask -> Op
crontask t task =
  op "crontask" (deps [run t task]) $ \actions -> actions {
    help = "setup " <> cmd <> " at " <> Text.pack path
  , ref = dotRef $ Text.pack $ "crontask:" <> path
  , up = up
  , down = removeFile path
  }
  where
    path :: FilePath
    path = platformCronPath </> Text.unpack (mconcat ["salmon-", task.name])

    cmd :: Text
    cmd = Text.pack task.command

    up :: IO ()
    up = ByteString.writeFile path $ Text.encodeUtf8 contents

    contents :: Text
    contents = Text.unlines 
      [ "# salmon-task: " <> task.name
      , renderTask task
      ]

    renderTask :: CronTask -> Text
    renderTask task =
      Text.unwords
        [ renderSchedule task.schedule
        , task.user
        , cmd
        , Text.unwords task.commandArgs
        ]

    renderSchedule :: Schedule -> Text
    renderSchedule sched =
      Text.unwords
        [ sched.minute
        , sched.hour
        , sched.dayOfMonth
        , sched.month
        , sched.dayOfWeek
        ]
