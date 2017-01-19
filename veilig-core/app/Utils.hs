module Utils ( clearHandle
             , notebookFilePath) where

import GHC.IO.Handle
import System.IO
import Types
import System.FilePath
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
