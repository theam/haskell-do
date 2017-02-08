{-# LANGUAGE OverloadedStrings #-}

module Utils ( clearHandle
             , defaultPrograms
             , loadNotebookFromFile
             , constructCells
             , constructNotebook ) where

import GHC.IO.Handle
import System.IO
import Types
import System.FilePath
import System.Process
import Data.IORef
import Language.Haskell.GhcMod
import Language.Haskell.GhcMod.Types
import Data.List (intercalate)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.List

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

constructCells :: [String] -> Int -> [Cell] -> [Cell]
constructCells [] _ curr = map (\c -> c { cellContent = T.strip $ cellContent c}) filtered
  where filtered = filter (\c -> (T.stripStart $ cellContent c) /=  (T.pack "")) curr
constructCells lst@(x:xs) i curr =
  if isPrefixOf "--" x then
    constructCells lst' (i+1) (curr ++ [textcell])
  else
    constructCells lst'' (i+1) (curr ++ [codecell])
  where
    lst' = dropWhile (\x -> isPrefixOf "--" x) lst
    textcell = Cell TextCell i (T.pack $ unlines $ map (\('-':'-':' ':xs) -> xs) $ takeWhile (\x -> isPrefixOf "--" x) lst)
    lst'' = dropWhile (\x -> not $ isPrefixOf "--" x) lst
    codecell = Cell CodeCell i (T.pack $ unlines $ takeWhile (\x -> not $ isPrefixOf "--" x) lst)

constructNotebook :: FilePath -> String -> Notebook
constructNotebook fp t = Notebook
  { title = ""
  , subtitle = ""
  , date = ""
  , author = ""
  , cells = (constructCells (lines t) 0 [])
  , console = "> "
  , filepath = fp
  , loaded = False
  }

loadNotebookFromFile :: FilePath -> IO Notebook
loadNotebookFromFile fp = do
  file <- readFile fp
  return $ constructNotebook fp file
