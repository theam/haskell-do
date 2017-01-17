module Utils (clearHandle) where

import GHC.IO.Handle
import System.IO
import System.Process

clearHandle :: Handle -> IO String
clearHandle out = do
  test <- hReady out
  if test
    then do x <- hGetChar out
            xs <- clearHandle out
            return (x : xs)
    else pure []
