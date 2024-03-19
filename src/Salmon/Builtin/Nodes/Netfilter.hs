module Salmon.Builtin.Nodes.Netfilter where

import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Binary
import qualified Salmon.Builtin.Nodes.Filesystem as FS
import Salmon.Op.Ref
import Salmon.Op.Track

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

table :: Track' (Binary "nft") -> Table -> Op
table nft t =
    withBinary nft nftcommand cmd $ \add ->
        op "nft-table" nodeps $ \actions ->
            actions
                { help = "creates an Netfilter table"
                , ref = dotRef $ "nft-table:" <> t.tableName
                , up = add
                , dynamics = [toDyn cmd]
                }
  where
    cmd = AddTable t

chain :: Track' (Binary "nft") -> Chain -> Op
chain nft c =
    withBinary nft nftcommand cmd $ \add ->
        op "nft-chain" (deps [table nft c.chainTable]) $ \actions ->
            actions
                { help = "creates an Netfilter chain"
                , ref = dotRef $ "nft-chain:" <> c.chainTable.tableName <> c.chainName
                , up = add
                , dynamics = [toDyn cmd]
                }
  where
    cmd = AddChain c

rule :: Track' (Binary "nft") -> Chain -> Rule -> Op
rule nft c r =
    withBinary nft nftcommand cmd $ \add ->
        op "nft-rule" (deps [chain nft c]) $ \actions ->
            actions
                { help = "creates an Netfilter rule"
                , ref = dotRef $ "nft-rule:" <> c.chainTable.tableName <> c.chainName <> textual r
                , up = add
                , dynamics = [toDyn cmd]
                }
  where
    cmd = AddRule c r
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
