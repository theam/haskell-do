{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Types where

import GHC.Generics
import Data.Aeson
import Data.Text

data Notebook = Notebook
    { title :: String
    , subtitle :: String
    , date :: String
    , author :: String
    , cells :: [Cell]
    } deriving (Generic, Show)

instance FromJSON Notebook
instance ToJSON Notebook where
    toEncoding = genericToEncoding defaultOptions

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
