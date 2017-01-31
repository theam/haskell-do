{-# LANGUAGE OverloadedStrings, LambdaCase, FlexibleContexts #-}
module WebSocketServer where

import Types

import Data.Monoid (mappend, (<>))
import Data.Text (Text)
import Data.List (intercalate)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Concurrent
import Control.Monad (forever, (>=>))
import qualified Network.WebSockets as WS
import Network.WebSockets (Connection)
import Data.String.Conversions
import Data.Aeson hiding (defaultOptions)
import Interpreter
import Utils
import GHC.IO.Handle
import Language.Haskell.GhcMod as GH
import Language.Haskell.GhcMod.Types
import System.IO
import System.IO.Unsafe
import System.Process
import Utils (defaultPrograms)

-- | Sets up the initial state
setupState :: FilePath -> IO (Handle, Handle, Handle, ProcessHandle)
setupState x = do
  (inp, out, err, pid) <- runInteractiveCommand "stack repl"
  hSetBinaryMode inp False
  hSetBinaryMode out False
  hSetBinaryMode err False
  hPutStrLn inp $ ":l " ++ x
  hPutStrLn inp "set prompt \">\""
  hFlush inp
  clearHandle out
  return (inp, out, err, pid)

initializeState :: FilePath -> IO State
initializeState x = do
  (inp, out, err, pid) <- setupState x
  (res, _) <- runGhcModT defaultOptions (findCradle defaultPrograms)
  case res of
    Right x ->
        pure $ State {
        ghciInput = inp
      , ghciOutput = out
      , ghciError = err
      , ghciProcessHandle = pid
      , notebookCradle = x }
    Left x -> error "No stack project found, please initialize a new project"

broadcast :: Connection -> Text -> IO ()
broadcast conn msg = do
  T.putStrLn ("Log:" <> cs msg)
  WS.sendTextData conn msg

application :: State -> WS.ServerApp
application state pending = do
  conn <- WS.acceptRequest pending
  talk conn state

distress conn = broadcast conn "Distress!"

broadcastNotebook conn n = broadcast conn (cs (encode n))

sendNotebook conn = either (broadcast conn . T.pack) (broadcastNotebook conn)

talk :: Connection -> State -> IO ()
talk conn state = forever $ do
  msg <- WS.receiveData conn
  maybe (distress conn)
        ((\notebook -> notebookInterpreter notebook state) >=> sendNotebook conn)
        (decode msg)
