module Salmon.Builtin.Nodes.Netfilter where

import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Binary (Binary, Command (..), withBinary)
import qualified Salmon.Builtin.Nodes.Binary as Binary
import qualified Salmon.Builtin.Nodes.Filesystem as FS
import Salmon.Op.Ref
import Salmon.Op.Track
import Salmon.Reporter

import System.IO (IOMode (ReadMode, WriteMode), withFile)

import Control.Monad (void)
import Data.Dynamic (toDyn)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import GHC.IO.Handle (Handle)

import System.FilePath (takeDirectory, (</>))
import System.Process (StdStream (UseHandle), waitForProcess)
import System.Process.ByteString (readCreateProcessWithExitCode)
import System.Process.ListLike (CreateProcess (..), proc)

-------------------------------------------------------------------------------
data Report
    = RunNftCommand !NftCommand !Binary.Report

-------------------------------------------------------------------------------

data NetFamily
    = Inet

-- TODO: Ip, Ip6, Arp, Bridge, NetDev

netFamily :: NetFamily -> Text
netFamily Inet = "inet"

data Table
    = Table
    { tableName :: Text
    , tableFamily :: NetFamily
    }

data Chain
    = Chain
    { chainName :: Text
    , chainTable :: Table
    }

data Rule
    = RawRule [Text]

data NftCommand
    = AddTable Table
    | AddChain Chain
    | AddRule Chain Rule

table :: Reporter Report -> Track' (Binary "nft") -> Table -> Op
table r nft t =
    withBinary nft nftcommand cmd $ \add ->
        op "nft-table" nodeps $ \actions ->
            actions
                { help = "creates an Netfilter table"
                , ref = dotRef $ "nft-table:" <> t.tableName
                , up = add r'
                , dynamics = [toDyn cmd]
                }
  where
    r' = contramap (RunNftCommand cmd) r
    cmd = AddTable t

chain :: Reporter Report -> Track' (Binary "nft") -> Chain -> Op
chain r nft c =
    withBinary nft nftcommand cmd $ \add ->
        op "nft-chain" (deps [table r nft c.chainTable]) $ \actions ->
            actions
                { help = "creates an Netfilter chain"
                , ref = dotRef $ "nft-chain:" <> c.chainTable.tableName <> c.chainName
                , up = add r'
                , dynamics = [toDyn cmd]
                }
  where
    r' = contramap (RunNftCommand cmd) r
    cmd = AddChain c

rule :: Reporter Report -> Track' (Binary "nft") -> Chain -> Rule -> Op
rule r nft c nftrule =
    withBinary nft nftcommand cmd $ \add ->
        op "nft-rule" (deps [chain r nft c]) $ \actions ->
            actions
                { help = "creates an Netfilter rule"
                , ref = dotRef $ "nft-rule:" <> c.chainTable.tableName <> c.chainName <> textual nftrule
                , up = add r'
                , dynamics = [toDyn cmd]
                }
  where
    r' = contramap (RunNftCommand cmd) r
    cmd = AddRule c nftrule
    textual :: Rule -> Text
    textual (RawRule txts) = Text.unwords txts

nftcommand :: Command "nft" NftCommand
nftcommand = Command $ \cmd -> case cmd of
    (AddTable table) ->
        proc
            "nft"
            [ "add"
            , "table"
            , Text.unpack $ netFamily table.tableFamily
            , Text.unpack table.tableName
            ]
    (AddChain chain) ->
        proc
            "nft"
            [ "add"
            , "chain"
            , Text.unpack $ netFamily chain.chainTable.tableFamily
            , Text.unpack chain.chainTable.tableName
            , Text.unpack chain.chainName
            ]
    (AddRule chain (RawRule terms)) ->
        proc "nft" $
            mconcat
                [
                    [ "add"
                    , "rule"
                    , Text.unpack $ netFamily chain.chainTable.tableFamily
                    , Text.unpack chain.chainTable.tableName
                    , Text.unpack chain.chainName
                    ]
                , fmap Text.unpack terms
                ]

-- todo: ruleset turning chains into single-run
