{-# LANGUAGE OverloadedStrings, LambdaCase #-}
module WebSocketServer where

import Types

import Language.Haskell.Interpreter
import Data.Char (isPunctuation, isSpace)
import Data.Monoid (mappend, (<>))
import Data.Text (Text)
import Control.Concurrent.STM
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Concurrent
import Control.Monad (forM_, forever, (>=>))
import Control.Exception (finally)
import qualified Network.WebSockets as WS
import Data.String.Conversions
import Data.Aeson
import Interpreter

type Client = WS.Connection
type ServerState = MVar Client

newServer :: IO ServerState
newServer = newEmptyMVar

broadcast :: Text -> Client -> IO ()
broadcast msg conn = do
    T.putStrLn ("Writing " <> msg)
    WS.sendTextData conn msg

application :: ServerState -> WS.ServerApp
application state pending = do
    conn <- WS.acceptRequest pending
    WS.forkPingThread conn 30
    msg <- WS.receiveData conn
    client <- tryReadMVar state
    case client of
      Nothing -> putMVar state conn >> talk conn msg
      Just client -> talk client msg

hasCorrectPrefix :: Text -> Bool
hasCorrectPrefix msg = connectionPrefix `T.isPrefixOf` msg

connectionPrefix :: Text
connectionPrefix = "HaskellDO:"

distress = broadcast "Could not decode"

sendNotebook conn = \case 
  Left err -> broadcast (cs (show err)) conn
  Right notebook -> broadcast (cs (encode notebook)) conn

talk :: Client -> Text -> IO ()
talk conn msg = forever $ do
    msg <- WS.receiveData conn
    maybe (distress conn) (notebookInterpreter >=> sendNotebook conn) (decode msg)
