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

constructCells :: [T.Text] -> Int -> [Cell] -> [Cell]
constructCells [] _ curr = curr
constructCells lst@(x:xs) i curr =
  if T.isPrefixOf "--" x then
    constructCells lst' (i+1) (curr ++ [textcell])
  else
    constructCells lst'' (i+1) (curr ++ [codecell])
  where
    lst' = dropWhile (\x -> T.isPrefixOf "--" x) lst
    textcell = Cell TextCell i (T.unlines $ takeWhile (\x -> T.isPrefixOf "--" x) lst)
    lst'' = dropWhile (\x -> not $ T.isPrefixOf "--" x) lst
    codecell = Cell CodeCell i (T.unlines $ takeWhile (\x -> not $ T.isPrefixOf "--" x) lst)

constructNotebook :: FilePath -> T.Text -> Notebook
constructNotebook fp t = Notebook
  { title = ""
  , subtitle = ""
  , date = ""
  , author = ""
  , cells = constructCells (T.lines t) 0 []
  , console = "> "
  , filepath = fp
  }

loadNotebookFromFile :: FilePath -> IO Notebook
loadNotebookFromFile fp = do
  file <- T.readFile fp
  return $ constructNotebook fp file
