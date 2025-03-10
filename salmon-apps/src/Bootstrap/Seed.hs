module Bootstrap.Seed where

import Data.Text (Text)
import Options.Applicative (command, commandGroup, fullDesc, header, help, helper, info, long, many, progDesc, strOption, subparser, value, (<**>))

import Options.Generic (ParseRecord (..))

data Seed
    = Seed
    { bootstrap_image :: FilePath
    }

instance ParseRecord Seed where
    parseRecord =
        combo <**> helper
      where
        combo =
            subparser $
                mconcat
                    [ commandGroup "bootstrap"
                    , command
                        "image"
                        (info build (header "Bootstrap" <> fullDesc <> progDesc description))
                    ]
        description :: String
        description =
            unlines
                [ "Bootstraps some image."
                ]
        build =
            Seed
                <$> strOption
                    (long "image" <> Options.Applicative.help "image name" <> value "salmon/seed-image")
