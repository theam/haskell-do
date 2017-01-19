{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Types where

import GHC.Generics
import Data.Aeson
import GHC.IO.Handle
import System.Process
import Data.Text

newtype Directory = Directory { getDir :: String }
newtype ProjectName = ProjectName { getProjName :: String }

data State = State {
    ghciInput :: Handle
  , ghciOutput :: Handle
  , ghciError :: Handle
  , ghciProcessHandle :: ProcessHandle
  , notebookProjectName :: ProjectName
  , notebookDirectory :: Directory
  , notebookAuthor :: Maybe String }

data Notebook = Notebook
    { title :: String
    , subtitle :: String
    , date :: String
    , author :: String
    , cells :: [Cell]
    , console :: String
    } deriving (Generic, Show)

instance FromJSON Notebook
instance ToJSON Notebook where
    toEncoding = genericToEncoding defaultOptions

data ProjectAction
  = OpenProject Directory
  | NewProject ProjectName Directory

data Cell = Cell
    { cellType :: CellType
    , cellId :: Int
    , cellContent :: Text
    } deriving (Generic, Show)

instance FromJSON Cell
instance ToJSON Cell where
    toEncoding = genericToEncoding defaultOptions

data CellType
    = TextCell
    | CodeCell
    | DisplayCell
    deriving (Generic, Show, Eq)

instance FromJSON CellType
instance ToJSON CellType where
    toEncoding = genericToEncoding defaultOptions
