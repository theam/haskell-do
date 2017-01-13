{-# LANGUAGE OverloadedStrings, LambdaCase, FlexibleContexts #-}
module WebSocketServer where

import Types

import Language.Haskell.Interpreter
import Data.Monoid (mappend, (<>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Concurrent
import Control.Monad (forever, (>=>))
import qualified Network.WebSockets as WS
import Network.WebSockets (Connection)
import Data.String.Conversions
import Data.Aeson
import Interpreter
import GHC
import GHC.IO.Handle
import System.IO
import System.Process

data State = State { 
    ghciInput :: Handle
  , ghciOutput :: Handle
  , ghciError :: Handle
  , ghciProcessHandle :: ProcessHandle 
  , notebookAuthor :: String }

type ServerState = (MVar State, Connection)

broadcast :: Connection -> Text -> IO ()
broadcast conn msg = do
  T.putStrLn ("Log:" <> cs msg)
  WS.sendTextData conn msg

application :: WS.ServerApp
application pending = do
  conn <- WS.acceptRequest pending
  (inp, out, err, pid) <- runInteractiveCommand "stack ghci"
  hSetBinaryMode inp False
  hSetBinaryMode out False
  hSetBinaryMode err False
  talk conn inp out

distress conn = broadcast conn "Distress!"

broadcastNotebook conn n = broadcast conn (cs (encode n))

sendNotebook conn = either (broadcast conn . T.pack) (broadcastNotebook conn)

talk :: Connection -> Handle -> Handle -> IO ()
talk conn inp out = forever $ do
  msg <- WS.receiveData conn
  maybe (distress conn) 
        ((\notebook -> notebookInterpreter notebook inp out) >=> sendNotebook conn) 
        (decode msg)
