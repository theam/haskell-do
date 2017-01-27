module Utils ( clearHandle
             , processAction
             , setupState
             , loadProject
             , defaultPrograms
             , createNewProject ) where

import GHC.IO.Handle
import System.IO
import Types
import System.FilePath
import System.Process
import Data.IORef
import Language.Haskell.GhcMod
import Language.Haskell.GhcMod.Types
import Data.List (intercalate)

-- | Clears the handle of any available input and returns it
clearHandle :: Handle -> IO String
clearHandle out = do
  test <- hReady out
  if test
    then do x <- hGetChar out
            xs <- clearHandle out
            return (x : xs)
    else pure []

defaultPrograms :: Programs
defaultPrograms =
  Programs { ghcProgram = "ghc"
           , ghcPkgProgram = "ghc-pkg"
           , cabalProgram = "cabal"
           , stackProgram = "stack" }

createNewProject :: ProjectName -> IO ()
createNewProject sv pn = do
  runInteractiveCommand $ unwords ["stack", "new", getProjName pn]
  runInteractiveCommand $ "cd " ++ (getProjName pn)
