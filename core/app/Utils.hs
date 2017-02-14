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
import System.Directory
import System.Process
import Data.IORef
import Language.Haskell.GhcMod
import Language.Haskell.GhcMod.Types
import Data.List (intercalate)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.List
import Data.List.Split

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

listofCellContentLines :: [String] -> [[String]]
listofCellContentLines = split whenLineIsFromATextCell
  where
    whenLineIsFromATextCell = dropBlanks . condense $ whenElt (isPrefixOf "--")

indexedCellContents :: [[String]] -> [(Int, [String])]
indexedCellContents cellContentLines = zip [0..length cellContentLines] cellContentLines

buildCell :: (Int, [String]) -> Cell
buildCell (cellId, cellContent)
  | cellContent `areFrom` TextCell = Cell TextCell cellId $ processedContents cellContent
  | cellContent`areFrom` CodeCell = Cell CodeCell cellId $ processedContents cellContent
  where
    areFrom cc TextCell  = any (T.isPrefixOf "--" . T.stripStart . T.pack) cc
    areFrom cc CodeCell  = any (not . isPrefixOf "--") cc
    processedContents = T.pack . unlines . map removeCommentCharacters
    removeCommentCharacters line = if "--" `isPrefixOf` line then drop 3 line else line
    

killEmptyCells :: Cell -> Bool
killEmptyCells c = T.stripStart (cellContent c) /= T.pack ""

constructCells :: [String] -> [Cell]
constructCells = map buildCell
               . indexedCellContents
               . listofCellContentLines

constructNotebook :: FilePath -> String -> Notebook
constructNotebook fp t = Notebook
  { title = ""
  , subtitle = ""
  , date = ""
  , author = ""
  , cells = filter killEmptyCells $ constructCells $ lines t
  , console = "> "
  , filepath = fp
  , loaded = False
  }

loadNotebookFromFile :: FilePath -> IO Notebook
loadNotebookFromFile fp = do
  exists <- doesFileExist fp
  if exists then do
    file <- readFile fp
    return $ constructNotebook fp file
  else do
    writeFile fp ""
    file <- readFile fp
    return $ constructNotebook fp file
