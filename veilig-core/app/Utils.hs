module Utils ( clearHandle
             , notebookFilePath) where

import GHC.IO.Handle
import System.IO
import Types
import System.FilePath
import System.Process
import Data.List (intercalate)

clearHandle :: Handle -> IO String
clearHandle out = do
  test <- hReady out
  if test
    then do x <- hGetChar out
            xs <- clearHandle out
            return (x : xs)
    else pure []

notebookFilePath :: State -> FilePath
notebookFilePath note = intercalate [pathSeparator] [getDir $ notebookDirectory note, "app", "Main.hs"]

processAction :: ProjectAction -> IO State
processAction pa = case pa of
  OpenProject dir -> loadProject dir
  NewProject pn dir -> createNewProject pn dir

setupState :: IO (Handle, Handle, Handle, ProcessHandle)
setupState = do
  (inp, out, err, pid) <- runInteractiveCommand "stack repl"
  hSetBinaryMode inp False
  hSetBinaryMode out False
  hSetBinaryMode err False
  hPutStrLn inp $ ":l " ++ (intercalate [pathSeparator] ["app", "Main.hs"])
  hPutStrLn inp "set prompt \">\""
  hFlush inp
  clearHandle out
  return (inp, out, err, pid)

createNewProject :: ProjectName -> Directory -> IO State
createNewProject pn dir = do
  runInteractiveCommand $ unwords ["stack", "new", getProjName pn]
  runInteractiveCommand $ "cd " ++ (getProjName pn)
  (inp, out, err, pid) <- setupState
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
  (inp, out, err, pid) <- setupState
  return (State {
    ghciInput = inp
  , ghciOutput = out
  , ghciError = err
  , ghciProcessHandle = pid
  , notebookProjectName = ProjectName $ takeFileName $ getDir dir
  , notebookDirectory = dir
  , notebookAuthor = Nothing })
