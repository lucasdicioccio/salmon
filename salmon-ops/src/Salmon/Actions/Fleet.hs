{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | The fleet fold (milestone 5 of @specs/pull-mode.md@): what
@salmon-fleet status DIR@ computes over a directory of status sink
documents ("Salmon.Actions.Serve.StatusSink"), one per host.

Fleet status is a fold over the documents the hosts wrote, computed by
whoever reads the directory — this module, a script, a web page — and not
by a running service: the directory is the only shared thing, and it is a
dumb one. The fold is pure ('fold') over what 'readStatusDir' found, so
that it is testable without a host, and the binary in @salmon-apps@ is a
thin command line over the two.

What a row says about a host: its name and mode, the document each of its
labels last applied (id and digest), how many of its nodes have converged
and how many are errored out of how many, and how long ago it wrote —
flagged 'rowStale' past a threshold. A stale host is a /visible fact/, not
a decision: nothing here decides a host is dead (see the spec's "what this
does not solve"), it only says nobody has heard from it lately.
-}
module Salmon.Actions.Fleet (
    -- * Reading a directory
    readStatusDir,

    -- * The fold
    Options (..),
    defaultOptions,
    Row (..),
    fold,
    rowOf,
    nodeCounts,

    -- * Rendering
    renderRow,
    renderHeader,
    rowValue,
) where

import Control.Exception (SomeException, try)
import Data.Aeson (ToJSON (..), Value (..), eitherDecode, object, (.=))
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as LByteString
import Data.List (isSuffixOf, sortOn)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime)
import System.Directory (listDirectory)
import System.FilePath ((</>))

import Salmon.Actions.Serve (AppliedDocument (..))
import Salmon.Actions.Serve.StatusSink (Document (..))

-------------------------------------------------------------------------------

{- | Every @*.json@ in the directory, read and parsed: the documents that
are status sink documents, and, by file, why the others are not. A file
that cannot be read at all is in the second list too; nothing is ever
written. -}
readStatusDir :: FilePath -> IO ([(FilePath, Document)], [(FilePath, String)])
readStatusDir dir = do
    names <- filter (".json" `isSuffixOf`) <$> listDirectory dir
    outcomes <- mapM readOne (sortOn id names)
    pure ([(f, d) | (f, Right d) <- outcomes], [(f, e) | (f, Left e) <- outcomes])
  where
    readOne name = do
        let path = dir </> name
        attempt <- try (LByteString.readFile path >>= \b -> LByteString.length b `seq` pure b)
        pure . (,) path $ case attempt of
            Left (ex :: SomeException) -> Left (show ex)
            Right bytes -> eitherDecode bytes

-------------------------------------------------------------------------------

data Options = Options
    { optLabel :: Maybe Text
    -- ^ only hosts carrying this label
    , optStale :: NominalDiffTime
    -- ^ a document written longer ago than this flags its host
    }
    deriving (Show, Eq)

-- | No label filter; stale after a minute.
defaultOptions :: Options
defaultOptions = Options Nothing 60

-- | One host, as the fold reports it.
data Row = Row
    { rowHost :: !Text
    , rowMode :: !Text
    , rowLabels :: [AppliedDocument]
    , rowConverged :: !Int
    , rowErrored :: !Int
    , rowNodes :: !Int
    , rowWritten :: !UTCTime
    , rowAge :: !NominalDiffTime
    -- ^ how long before @now@ the document was written; negative for a
    -- clock ahead of the reader's
    , rowStale :: !Bool
    , rowFile :: !FilePath
    }
    deriving (Show, Eq)

{- | The fold: one row per document, hosts in name order, filtered to the
label asked for, each flagged stale against @now@. Two documents naming
one host (two files, one machine) are two rows: the fold reports what is
there and does not pick. -}
fold :: Options -> UTCTime -> [(FilePath, Document)] -> [Row]
fold opts now docs =
    sortOn (\r -> (r.rowHost, r.rowFile))
        [ row
        | (path, doc) <- docs
        , maybe True (\l -> l `elem` fmap (.appliedDocLabel) doc.docLabels) opts.optLabel
        , let row = rowOf opts now path doc
        ]

rowOf :: Options -> UTCTime -> FilePath -> Document -> Row
rowOf opts now path doc =
    Row
        { rowHost = doc.docHost
        , rowMode = doc.docMode
        , rowLabels = doc.docLabels
        , rowConverged = converged
        , rowErrored = errored
        , rowNodes = total
        , rowWritten = doc.docWritten
        , rowAge = age
        , rowStale = age > opts.optStale
        , rowFile = path
        }
  where
    age = now `diffUTCTime` doc.docWritten
    (converged, errored, total) = nodeCounts doc.docStatus

{- | Converged, errored, total, read off the @status@ object's @nodes@ —
the same objects @status --json@ prints. Anything that is not that shape
counts as no nodes. -}
nodeCounts :: Value -> (Int, Int, Int)
nodeCounts status =
    case status of
        Object o | Just (Array nodes) <- KeyMap.lookup "nodes" o ->
            let convergences = [c | Object n <- foldr (:) [] nodes, Just (String c) <- [KeyMap.lookup "convergence" n]]
             in ( length (filter (== "converged") convergences)
                , length (filter (== "errored") convergences)
                , length convergences
                )
        _ -> (0, 0, 0)

-------------------------------------------------------------------------------

-- | The column names, for the line above 'renderRow's.
renderHeader :: Text
renderHeader = "host\tmode\tlabels\tconverged\terrored\tage\tflags"

{- | One line per host, tab-separated: host, mode, @label=id@sha256[:12]@
per label (comma-separated, @-@ for none), @converged/total@, errored,
the document's age in seconds, and @stale@ or nothing. -}
renderRow :: Row -> Text
renderRow r =
    Text.intercalate
        "\t"
        [ r.rowHost
        , r.rowMode
        , if null r.rowLabels then "-" else Text.intercalate "," (fmap labelText r.rowLabels)
        , Text.pack (show r.rowConverged) <> "/" <> Text.pack (show r.rowNodes)
        , Text.pack (show r.rowErrored)
        , Text.pack (show (round r.rowAge :: Integer)) <> "s"
        , if r.rowStale then "stale" else ""
        ]
  where
    labelText :: AppliedDocument -> Text
    labelText a = a.appliedDocLabel <> "=" <> a.appliedDocId <> "@" <> Text.take 12 a.appliedDocDigest

-- | The row as @--json@ prints it.
rowValue :: Row -> Value
rowValue r =
    object
        [ "host" .= r.rowHost
        , "mode" .= r.rowMode
        , "labels" .= r.rowLabels
        , "converged" .= r.rowConverged
        , "errored" .= r.rowErrored
        , "nodes" .= r.rowNodes
        , "written" .= r.rowWritten
        , "age_s" .= (realToFrac r.rowAge :: Double)
        , "stale" .= r.rowStale
        , "file" .= r.rowFile
        ]

instance ToJSON Row where
    toJSON = rowValue
