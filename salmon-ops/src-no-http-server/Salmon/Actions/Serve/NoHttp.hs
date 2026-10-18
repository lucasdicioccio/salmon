{-# LANGUAGE EmptyCase #-}

{- | What "Salmon.Builtin.CommandLine" uses of "Salmon.Actions.Serve.Http",
for a @salmon-ops@ built with @-http-server@: the same names, a 'Server' that
cannot exist, and a 'withHttpServerOn' that refuses.

Hidden (an @other-module@) and named apart on purpose: code that imports
"Salmon.Actions.Serve.Http" fails to build without the flag, rather than
compiling against a server that never answers. The command line refuses
@--http@\/@--http-tcp@ before it gets here, so 'withHttpServerOn' is the
second line of defence, not the message an operator reads.
-}
module Salmon.Actions.Serve.NoHttp (
    module Salmon.Actions.Serve.HttpBind,
    Server,
    withHttpServerOn,
    serverObserver,
    serverProducer,
    serverReporters,
    serverFollowReporter,
    serverNodeLogReporter,
) where

import Data.Text (Text)

import Salmon.Actions.Serve (Producer, Mode)
import qualified Salmon.Actions.Serve.Events as Events
import Salmon.Actions.Serve.HttpBind
import Salmon.Reporter (Reporter)

-- | No value: without the flag there is no server to hand anything.
data Server

withHttpServerOn :: Events.Config -> [Bind] -> Text -> IO Mode -> (Server -> IO a) -> IO a
withHttpServerOn _ _ _ _ _ = ioError (userError "salmon-ops was built without its http-server flag: no HTTP listener")

serverObserver :: Server -> IO w -> IO ()
serverObserver s = case s of {}

serverProducer :: Server -> Producer
serverProducer s = case s of {}

serverReporters :: Server -> a -> a
serverReporters s = case s of {}

serverFollowReporter :: Server -> Reporter a
serverFollowReporter s = case s of {}

serverNodeLogReporter :: Server -> Reporter a
serverNodeLogReporter s = case s of {}
