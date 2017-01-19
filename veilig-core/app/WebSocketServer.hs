{-# LANGUAGE OverloadedStrings, LambdaCase, FlexibleContexts #-}
module WebSocketServer where

import Types

import Language.Haskell.Interpreter
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
import Data.Aeson
import Interpreter
import GHC
import Utils
import GHC.IO.Handle
import System.IO
import System.Process
import System.FilePath (pathSeparator, takeFileName)

broadcast :: Connection -> Text -> IO ()
broadcast conn msg = do
  T.putStrLn ("Log:" <> cs msg)
  WS.sendTextData conn msg

createNewProject :: ProjectName -> Directory -> IO State
createNewProject pn dir = do
  runInteractiveCommand $ unwords ["stack", "new", getProjName pn]
  runInteractiveCommand $ "cd " ++ (getProjName pn)
  (inp, out, err, pid) <- runInteractiveCommand "stack repl"
  hSetBinaryMode inp False
  hSetBinaryMode out False
  hSetBinaryMode err False
  hPutStrLn inp $ ":l " ++ (intercalate [pathSeparator] ["app", "Main.hs"])
  hPutStrLn inp "set prompt \">\""
  hFlush inp
  clearHandle out
  return $ State {
     ghciInput = inp
  , ghciOutput = out
  , ghciError = err
  , ghciProcessHandle = pid
  , notebookProjectName = pn
  , notebookDirectory = dir
  , notebookAuthor = Nothing
  }

loadProject :: Directory -> IO State
loadProject dir = do
  (inp, out, err, pid) <- runInteractiveCommand "stack repl"
  hSetBinaryMode inp False
  hSetBinaryMode out False
  hSetBinaryMode err False
  hPutStrLn inp (":l " ++ (intercalate [pathSeparator] ["app", "Main.hs"]))
  hPutStrLn inp "set prompt \">\""
  hFlush inp
  clearHandle out
  return (State {
    ghciInput = inp
  , ghciOutput = out
  , ghciError = err
  , ghciProcessHandle = pid
  , notebookProjectName = ProjectName $ takeFileName $ getDir dir
  , notebookDirectory = dir
  , notebookAuthor = Nothing })


initializeState :: String -> IO State
initializeState st = do
  (inp, out, err, pid) <- runInteractiveCommand "stack repl"
  hSetBinaryMode inp False
  hSetBinaryMode out False
  hSetBinaryMode err False
  hPutStrLn inp (":l " ++ (intercalate [pathSeparator] [st, "app", "Main.hs"]))
  hPutStrLn inp "set prompt \">\""
  hFlush inp
  clearHandle out
  return (State {
    ghciInput = inp
  , ghciOutput = out
  , ghciError = err
  , ghciProcessHandle = pid
  , notebookProjectName = ProjectName st
  , notebookDirectory = Directory st
  , notebookAuthor = Nothing })

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
