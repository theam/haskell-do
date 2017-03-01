{-# LANGUAGE OverloadedStrings, LambdaCase, FlexibleContexts #-}
module WebSocketServer where

import Types

import Data.Monoid (mappend, (<>))
import Data.Text (Text)
import Data.List (intercalate)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Concurrent
import Control.Monad (forever, (>=>), when)
import qualified Network.WebSockets as WS
import Network.WebSockets (Connection)
import Data.String.Conversions
import Data.Aeson hiding (defaultOptions)
import Interpreter
import Utils
import GHC.IO.Handle
import Language.Haskell.GhcMod as GH
import Language.Haskell.GhcMod.Types
import Data.Either.Unwrap
import System.IO
import System.IO.Unsafe
import System.Process
import System.Directory
import System.FilePath

-- | Sets up the initial state
setupState :: FilePath -> IO (Handle, Handle, Handle, ProcessHandle)
setupState x = do
  (inp, out, err, pid) <- runInteractiveCommand "stack repl"
  hSetBinaryMode inp False
  hSetBinaryMode out False
  hSetBinaryMode err False
  curDir <- getCurrentDirectory
  loadFileNotInStackProject curDir (takeDirectory x) inp
  --when (curDir == takeDirectory x) $ hPutStrLn inp $ ":l " ++ x -- if we're not in a stack project, load the file
  hFlush inp
  clearHandle out
  return (inp, out, err, pid)
  where
    loadFileNotInStackProject fdir sdir inp = when (fdir == sdir) $ hPutStrLn inp $ ":l " ++ x

initializeState :: FilePath -> IO State
initializeState x = do
  (res, _) <- runGhcModT defaultOptions (findCradle defaultPrograms)
  let cradle = fromRight res 
  setCurrentDirectory $ cradleRootDir cradle 
  (inp, out, err, pid) <- setupState x
  pure State {
  ghciInput = inp
  , ghciOutput = out
  , ghciError = err
  , ghciProcessHandle = pid
  , notebookCradle = cradle }

broadcast :: Connection -> Text -> IO ()
broadcast conn msg = do
  T.putStrLn ("Log:" <> cs msg)
  WS.sendTextData conn msg

application :: FilePath -> Notebook -> State -> WS.ServerApp
application fp nb state pending = do
  conn <- WS.acceptRequest pending
  broadcastNotebook conn nb
  talk conn state fp

distress conn = broadcast conn "Distress!"

broadcastNotebook conn n = broadcast conn (cs (encode n))

sendNotebook conn = either (broadcast conn . T.pack) (broadcastNotebook conn)

talk :: Connection -> State -> FilePath -> IO ()
talk conn state fp = forever $ do
  msg <- WS.receiveData conn
  maybe (distress conn)
        ((\notebook -> notebookInterpreter (notebook { filepath = fp }) state) >=> sendNotebook conn)
        (decode msg)
