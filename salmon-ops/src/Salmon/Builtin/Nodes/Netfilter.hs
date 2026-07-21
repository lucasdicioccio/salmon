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
    deriving (Show)

-------------------------------------------------------------------------------

data NetFamily
    = Inet
    deriving (Show)

-- TODO: Ip, Ip6, Arp, Bridge, NetDev

netFamily :: NetFamily -> Text
netFamily Inet = "inet"

data Table
    = Table
    { tableName :: Text
    , tableFamily :: NetFamily
    }
    deriving (Show)

{- | A base chain is one hooked into the netfilter packet path (as opposed to a
regular chain, which only runs when jumped to explicitly). Needed for e.g. NAT
postrouting or forwarding decisions to actually fire.
-}
data ChainType
    = FilterChain
    | NatChain
    deriving (Show)

chainTypeTxt :: ChainType -> Text
chainTypeTxt FilterChain = "filter"
chainTypeTxt NatChain = "nat"

data Hook
    = PreRouting
    | Input
    | Forward
    | Output
    | PostRouting
    deriving (Show)

hookTxt :: Hook -> Text
hookTxt PreRouting = "prerouting"
hookTxt Input = "input"
hookTxt Forward = "forward"
hookTxt Output = "output"
hookTxt PostRouting = "postrouting"

data ChainPolicy
    = Accept
    | Drop
    deriving (Show)

policyTxt :: ChainPolicy -> Text
policyTxt Accept = "accept"
policyTxt Drop = "drop"

data BaseChainSpec
    = BaseChainSpec
    { baseChainType :: ChainType
    , baseChainHook :: Hook
    , baseChainPriority :: Int
    , baseChainPolicy :: ChainPolicy
    }
    deriving (Show)

data Chain
    = Chain
    { chainName :: Text
    , chainTable :: Table
    , chainBase :: Maybe BaseChainSpec
    }
    deriving (Show)

-- | A regular (non-hooked) chain, only reachable via explicit jumps.
regularChain :: Text -> Table -> Chain
regularChain name t = Chain name t Nothing

-- | A base chain, hooked into the netfilter packet path.
baseChain :: Text -> Table -> BaseChainSpec -> Chain
baseChain name t spec = Chain name t (Just spec)

data Rule
    = RawRule [Text]
    deriving (Show)

data NftCommand
    = AddTable Table
    | AddChain Chain
    | AddRule Chain Rule
    deriving (Show)

table :: Reporter Report -> Track' (Binary "nft") -> Table -> Op
table r nft t =
    withBinary nft nftcommand cmd $ \add ->
        op "nft-table" nodeps $ \actions ->
            actions
                { help = "creates an Netfilter table"
                , ref = mkRef "nft-table" t.tableName
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
                , ref = mkRef "nft-chain" (c.chainTable.tableName, c.chainName)
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
                , ref = mkRef "nft-rule" (c.chainTable.tableName, c.chainName, textual nftrule)
                , up = add r'
                , dynamics = [toDyn cmd]
                }
  where
    r' = contramap (RunNftCommand cmd) r
    cmd = AddRule c nftrule
    textual :: Rule -> Text
    textual (RawRule txts) = Text.unwords txts

baseChainSpecTerms :: BaseChainSpec -> [Text]
baseChainSpecTerms spec =
    [ "{"
    , "type"
    , chainTypeTxt spec.baseChainType
    , "hook"
    , hookTxt spec.baseChainHook
    , "priority"
    , Text.pack (show spec.baseChainPriority)
    , ";"
    , "policy"
    , policyTxt spec.baseChainPolicy
    , ";"
    , "}"
    ]

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
        proc "nft" $
            mconcat
                [
                    [ "add"
                    , "chain"
                    , Text.unpack $ netFamily chain.chainTable.tableFamily
                    , Text.unpack chain.chainTable.tableName
                    , Text.unpack chain.chainName
                    ]
                , maybe [] (fmap Text.unpack . baseChainSpecTerms) chain.chainBase
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
