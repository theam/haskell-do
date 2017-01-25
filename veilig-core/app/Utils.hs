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

-- | Undertakes the given action, producing a new State
processAction :: StateVar -> ProjectAction -> IO ()
processAction sv pa = case pa of
  OpenProject -> loadProject sv
  NewProject pn -> createNewProject sv pn

-- | Sets up the initial state
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

loadProject :: StateVar -> IO ()
loadProject sv = do
  (inp, out, err, pid) <- setupState
  (res, _) <- runGhcModT defaultOptions (findCradle defaultPrograms)
  case res of
    Right x -> atomicWriteIORef sv $ State {
        ghciInput = inp
      , ghciOutput = out
      , ghciError = err
      , ghciProcessHandle = pid
      , notebookCradle = x }
    Left x -> print x


createNewProject :: StateVar -> ProjectName -> IO ()
createNewProject sv pn = do
  runInteractiveCommand $ unwords ["stack", "new", getProjName pn]
  runInteractiveCommand $ "cd " ++ (getProjName pn)
  loadProject sv
