module Types where

data Action
  = ToggleEdit

type Notebook =
  { title :: String
  , subtitle :: String
  , date :: String
  , author :: String
  , cells :: Array Cell
  }

data Cell
  = TextCell String
  | CodeCell String DisplayResult

newtype DisplayResult = DisplayResult String

type AppState =
  { editing :: Boolean
  , notebook :: Notebook
  , rawText :: String
  , renderedText :: String
  }
