{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | @salmon-tui PATH@: a terminal client against @run serve --http PATH@
(milestone 6 of @specs\/generic-server.md@); or @salmon-tui
https:\/\/HOST:PORT --token-file FILE [--cacert FILE]@ against @run serve
--http-tcp HOST:PORT@'s listener (milestone 8), presenting the token from
the file — the same file the server was given, refused if others can read
it — on every request and trusting only the certificate in @--cacert@ when
one is named (a self-signed one is pinned this way), the system's store
otherwise.

The whole client is "Salmon.Client.Http" for the socket and
"Salmon.Client.Model" for the state; this module is a @brick@ rendering
of the 'Model' and a keyboard over the client, kept thin on purpose so
that what a screen shows is what the model says and nothing more. It
reads @\/dag@ once, follows @\/events@ from that snapshot's @seq@, folds
each event into the model, and re-reads @\/dag@ (rebasing the model on
it) whenever the model asks — a @declared@ or a @gap@ — or when @r@ is
pressed. It holds no state the server does not: a restart is one @\/dag@
read.

= What touches the loop

Nothing here stands the tending machines down except the command line.
Every read bypasses the inbox (see "Salmon.Actions.Serve.Http"); only a
line typed after @:@ is a command, sent with @POST \/command?async@, and
the footer says so. The number the server queued it at is echoed, and
its reports arrive on the stream like everything else.

= Keys

  * @j@\/@k@ (or the arrows): move the cursor over the node table
  * @enter@: expand the selected node — its help, notes, paths, edges,
    last check and output ring — and collapse it again
  * @:@: type a serve command; @enter@ sends it asynchronously, @esc@
    drops it
  * @r@: re-read @\/dag@
  * @q@: quit (the server is left exactly as it was)

= The stream

A lost connection is retried after a second with @?since=@ the last
number the stream delivered; the header says @reconnecting@ meanwhile.
The server replays what its ring still holds and sends a @gap@ first when
it does not, which the model turns into a re-read.
-}
module Tui (main) where

import Brick
import Brick.BChan (BChan, newBChan, writeBChan)
import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (SomeException, displayException, fromException, try)
import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (isPrefixOf)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word64)
import qualified Graphics.Vty as Vty
import qualified Network.HTTP.Client as HTTP
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import qualified Salmon.Actions.Serve.Http as Http
import qualified Salmon.Client.Http as Client
import qualified Salmon.Client.Model as Model
import Salmon.Client.Model (Model, Node (..))

-------------------------------------------------------------------------------

data Name = Table | Detail
    deriving (Eq, Ord, Show)

-- | What the background threads tell the event loop.
data Msg
    = -- | a @\/dag@ answer, or why there is none
      Snapshot !(Either Text Model)
    | -- | one event off the stream
      Streamed !Model.Event
    | -- | the stream is open (again)
      StreamUp
    | -- | the stream ended or failed; retrying
      StreamLost !Text
    | -- | the answer to a command typed at the prompt
      Queued !Text !(Either Text Client.Enqueued)

data St = St
    { stTarget :: !String
    , stClient :: !Client.Client
    , stChan :: !(BChan Msg)
    , stModel :: !Model
    , stCursor :: !Int
    , stExpanded :: !Bool
    , stInput :: !(Maybe Text)
    -- ^ the command line, while one is being typed
    , stNotice :: !Text
    -- ^ the footer's message line
    , stStream :: !Text
    -- ^ @live@ or @reconnecting@
    }

main :: IO ()
main = do
    args <- getArgs
    client <- either (\err -> usage err >> exitFailure) id (clientFor args)
    first <- try (Client.dag client)
    model <- case first of
        Left (e :: SomeException) -> do
            hPutStrLn stderr ("salmon-tui: cannot read /dag on " <> Client.clientTarget client <> ": " <> describe e)
            exitFailure
        Right v -> either (\err -> hPutStrLn stderr ("salmon-tui: " <> err) >> exitFailure) pure (Model.fromDag v)
    chan <- newBChan 256
    cursor <- newIORef (Just model.modelSeq)
    _ <- forkIO (follow client chan cursor)
    let st0 =
            St
                { stTarget = Client.clientTarget client
                , stClient = client
                , stChan = chan
                , stModel = model
                , stCursor = 0
                , stExpanded = False
                , stInput = Nothing
                , stNotice = "reads bypass the loop; only a command typed after : stands the tending machines down"
                , stStream = "connecting"
                }
    void (customMainWithDefaultVty (Just chan) app st0)

{- | The client the arguments name: a socket path alone, or an @https@ URL
with the token file and optionally the certificate to pin. A token file
without a URL, or a URL without one, is refused rather than guessed at.
-}
clientFor :: [String] -> Either String (IO Client.Client)
clientFor args =
    case args of
        [path] | not (isUrl path) -> Right (Client.newUnixClient path)
        url : flags | isUrl url -> do
            opts <- flagsOf flags (Nothing, Nothing)
            case opts of
                (Nothing, _) -> Left (url <> " needs --token-file FILE: the listener answers nothing without the token")
                (Just tokenFile, caFile) -> Right $ do
                    token <- Http.readTokenFile tokenFile
                    case token of
                        Left (Http.TokenFileReadable p) -> die ("--token-file " <> p <> " is readable by others; a token anyone on the box can read is not one (chmod 600 it)")
                        Left (Http.TokenFileEmpty p) -> die ("--token-file " <> p <> " is empty")
                        Right tok -> do
                            r <- try (Client.newTlsClient (Client.TlsTarget url tok caFile))
                            either (\(e :: SomeException) -> die (describe e)) pure r
        _ -> Left ""
  where
    isUrl a = "https://" `isPrefixOf` a || "http://" `isPrefixOf` a
    flagsOf [] acc = Right acc
    flagsOf ("--token-file" : f : rest) (_, ca) = flagsOf rest (Just f, ca)
    flagsOf ("--cacert" : f : rest) (tok, _) = flagsOf rest (tok, Just f)
    flagsOf (other : _) _ = Left ("unexpected argument: " <> other)
    die msg = hPutStrLn stderr ("salmon-tui: " <> msg) >> exitFailure

{- | An exception as one line: a refusal or a bad address as what the server
or the client said, a connection failure as its cause alone —
@http-client@'s own rendering prints the whole request first, which is
twenty lines of nothing the reader asked about.
-}
describe :: SomeException -> String
describe e
    | Just (Client.Refused code err) <- fromException e = show code <> " " <> Text.unpack err
    | Just (Client.BadTarget err) <- fromException e = Text.unpack err
    | Just (Client.Undecodable err) <- fromException e = Text.unpack err
    | Just (HTTP.HttpExceptionRequest _ content) <- fromException e = show content
    | otherwise = displayException e

usage :: String -> IO ()
usage err = do
    prog <- getProgName
    mapM_ (hPutStrLn stderr) $
        [err | not (null err)]
            ++ [ "usage: " <> prog <> " PATH                                            (the socket `run serve --http PATH` listens on)"
               , "       " <> prog <> " https://HOST:PORT --token-file FILE [--cacert FILE]   (`run serve --http-tcp HOST:PORT`)"
               ]

-------------------------------------------------------------------------------
-- the threads

{- | Follow the stream forever, from the last number delivered. The cursor
starts at the snapshot's @seq@ and is moved by every event that carries
one; a gap carries none and leaves it where it was, so a reconnect after
a gap asks for the same range again and gets the same gap, which is
right — the model re-reads on each.
-}
follow :: Client.Client -> BChan Msg -> IORef (Maybe Word64) -> IO ()
follow client chan cursor = forever $ do
    since <- readIORef cursor
    r <- try $ do
        writeBChan chan StreamUp
        Client.events client since Client.noFilter $ \e -> do
            mapM_ (writeIORef cursor . Just) e.eventSeq
            writeBChan chan (Streamed e)
            pure True
    case r of
        Left (e :: SomeException) -> writeBChan chan (StreamLost (Text.pack (describe e)))
        Right () -> writeBChan chan (StreamLost "the stream ended")
    threadDelay 1000000

-- | Read @\/dag@ on its own thread; the answer arrives as a 'Snapshot'.
refresh :: St -> IO ()
refresh st = void . forkIO $ do
    r <- try (Client.dag st.stClient)
    writeBChan st.stChan . Snapshot $ case r of
        Left (e :: SomeException) -> Left (Text.pack (describe e))
        Right v -> either (Left . Text.pack) Right (Model.fromDag v)

-- | Send a line asynchronously; the answer arrives as 'Queued'.
send :: St -> Text -> IO ()
send st line = void . forkIO $ do
    r <- try (Client.commandAsync st.stClient line)
    writeBChan st.stChan . Queued line $ case r of
        Left (e :: SomeException) -> Left (Text.pack (describe e))
        Right q -> Right q

-------------------------------------------------------------------------------
-- the app

app :: App St Msg Name
app =
    App
        { appDraw = draw
        , appChooseCursor = neverShowCursor
        , appHandleEvent = handle
        , appStartEvent = pure ()
        , appAttrMap = const theme
        }

theme :: AttrMap
theme =
    attrMap
        Vty.defAttr
        [ (attrName "selected", Vty.black `on` Vty.white)
        , (attrName "header", Vty.withStyle Vty.defAttr Vty.bold)
        , (attrName "converged", fg Vty.green)
        , (attrName "errored", fg Vty.red)
        , (attrName "blocked", fg Vty.yellow)
        , (attrName "stale", fg Vty.yellow)
        , (attrName "down", fg Vty.magenta)
        , (attrName "notice", fg Vty.cyan)
        , (attrName "prompt", Vty.withStyle Vty.defAttr Vty.bold)
        ]

handle :: BrickEvent Name Msg -> EventM Name St ()
handle ev = case ev of
    AppEvent msg -> onMsg msg
    VtyEvent (Vty.EvKey key mods) -> do
        typing <- gets stInput
        case typing of
            Just line -> onPromptKey line key mods
            Nothing -> onKey key
    _ -> pure ()

onMsg :: Msg -> EventM Name St ()
onMsg msg = case msg of
    Snapshot (Left err) -> modify $ \st -> st{stNotice = "/dag: " <> err}
    Snapshot (Right fresh) -> modify $ \st ->
        let m = Model.rebase st.stModel fresh
         in st{stModel = m, stCursor = clampCursor m st.stCursor, stNotice = "re-read /dag at seq " <> tshow m.modelSeq}
    Streamed e -> do
        st <- get
        let m = Model.step st.stModel e
        modify $ \s -> s{stModel = m, stCursor = clampCursor m s.stCursor, stNotice = Model.renderEventLine e}
        -- a declaration or a gap: the picture may have changed shape
        case Model.modelResync m of
            Just why -> do
                modify $ \s -> s{stModel = Model.resolve m, stNotice = why <> "; re-reading /dag"}
                liftIO (refresh st)
            Nothing -> pure ()
    StreamUp -> modify $ \st -> st{stStream = "live"}
    StreamLost why -> modify $ \st -> st{stStream = "reconnecting", stNotice = "stream: " <> why}
    Queued line (Left err) -> modify $ \st -> st{stNotice = "refused: " <> line <> ": " <> err}
    Queued line (Right q) -> modify $ \st -> st{stNotice = "queued at seq " <> tshow q.enqueuedSeq <> " (" <> q.enqueuedOrigin <> "): " <> line}

onKey :: Vty.Key -> EventM Name St ()
onKey key = case key of
    Vty.KChar 'q' -> halt
    Vty.KChar 'j' -> move 1
    Vty.KDown -> move 1
    Vty.KChar 'k' -> move (-1)
    Vty.KUp -> move (-1)
    Vty.KChar 'g' -> modify $ \st -> st{stCursor = 0}
    Vty.KChar 'G' -> modify $ \st -> st{stCursor = max 0 (length (Model.nodesInOrder st.stModel) - 1)}
    Vty.KEnter -> modify $ \st -> st{stExpanded = not st.stExpanded}
    Vty.KChar 'r' -> do
        st <- get
        liftIO (refresh st)
        modify $ \s -> s{stNotice = "re-reading /dag"}
    Vty.KChar ':' -> modify $ \st -> st{stInput = Just ""}
    _ -> pure ()
  where
    move :: Int -> EventM Name St ()
    move d = modify $ \st -> st{stCursor = clampCursor st.stModel (st.stCursor + d)}

onPromptKey :: Text -> Vty.Key -> [Vty.Modifier] -> EventM Name St ()
onPromptKey line key mods = case key of
    Vty.KEsc -> modify $ \st -> st{stInput = Nothing, stNotice = "command dropped"}
    Vty.KEnter
        | Text.null (Text.strip line) -> modify $ \st -> st{stInput = Nothing}
        | otherwise -> do
            st <- get
            liftIO (send st (Text.strip line))
            modify $ \s -> s{stInput = Nothing, stNotice = "sending: " <> Text.strip line}
    Vty.KBS -> modify $ \st -> st{stInput = Just (Text.dropEnd 1 line)}
    Vty.KChar 'u' | Vty.MCtrl `elem` mods -> modify $ \st -> st{stInput = Just ""}
    Vty.KChar c | null mods || mods == [Vty.MShift] -> modify $ \st -> st{stInput = Just (Text.snoc line c)}
    _ -> pure ()

clampCursor :: Model -> Int -> Int
clampCursor m i = max 0 (min i (length (Model.nodesInOrder m) - 1))

-------------------------------------------------------------------------------
-- drawing

draw :: St -> [Widget Name]
draw st = [vBox [header, table, detail, footer]]
  where
    m = st.stModel
    nodes = Model.nodesInOrder m

    header =
        withAttr (attrName "header") . padRight Max . txt $
            Model.renderHeader (Text.pack st.stTarget) m <> " stream=" <> st.stStream

    columns = Text.unwords [pad 10 "ref", pad 22 "shorthand", pad 4 "dir", pad 9 "state", pad 12 "check", "last event"]
    pad n = Text.justifyLeft n ' '

    table =
        vBox
            [ withAttr (attrName "header") (padRight Max (txt ("  " <> columns)))
            , viewport Table Vertical $
                vBox
                    [ row i n
                    | (i, n) <- zip [0 ..] nodes
                    ]
            , when' (null nodes) (txt "  (no node declared)")
            ]

    row i n
        | i == st.stCursor = visible (withAttr (attrName "selected") (padRight Max (txt ("> " <> Model.renderNodeRow n))))
        | otherwise = withAttr (stateAttr n) (padRight Max (txt ("  " <> Model.renderNodeRow n)))

    stateAttr :: Node -> AttrName
    stateAttr n
        | n.nodeDirection == "down" = attrName "down"
        | otherwise = attrName (Text.unpack n.nodeConvergence)

    detail
        | not st.stExpanded = emptyWidget
        | otherwise = case drop st.stCursor nodes of
            (n : _) -> vLimit 14 (viewport Detail Vertical (vBox (fmap txtWrap (detailLines n))))
            [] -> emptyWidget

    footer =
        vBox
            [ withAttr (attrName "notice") (padRight Max (txt (Text.take 200 st.stNotice)))
            , case st.stInput of
                Just line -> withAttr (attrName "prompt") (padRight Max (txt (":" <> line <> "_")))
                Nothing -> padRight Max (txt "j/k move  enter expand  : command (async; stands the machines down)  r re-read /dag  q quit")
            ]

    when' c w = if c then w else emptyWidget

-- | The expanded view of one node: everything the model has about it.
detailLines :: Node -> [Text]
detailLines n =
    [ "ref: " <> n.nodeRef.refShort <> " (" <> n.nodeRef.refFull <> ")"
    , "shorthand: " <> n.nodeShorthand
    , "help: " <> n.nodeHelp
    ]
        ++ ["note: " <> t | t <- n.nodeNotes]
        ++ ["path: " <> p | p <- n.nodePaths]
        ++ ["depends on: " <> Text.unwords (fmap (.refShort) n.nodeDependencies) | not (null n.nodeDependencies)]
        ++ ["depended on by: " <> Text.unwords (fmap (.refShort) n.nodeDependants) | not (null n.nodeDependants)]
        ++ [ "check: " <> c.checkVerdict <> maybe "" (" — " <>) c.checkReason | Just c <- [n.nodeCheck] ]
        ++ ["error: " <> e | Just e <- [n.nodeError]]
        ++ ["last event: " <> k <> maybe "" (\s -> " #" <> tshow s) n.nodeLastSeq | Just k <- [n.nodeLastKind]]
        ++ case n.nodeOutput of
            [] -> ["output: (none in the last snapshot)"]
            ls -> "output (last snapshot):" : fmap ("  " <>) (lastN 10 ls)
  where
    lastN k xs = drop (max 0 (length xs - k)) xs

tshow :: (Show a) => a -> Text
tshow = Text.pack . show
