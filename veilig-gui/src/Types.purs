module Types where

import Data.Lens

data Action
  = ToggleEdit
  | AddTextCell
  | CheckInput String

type AppState =
  { editing :: Boolean
  , notebook :: Notebook
  , rawText :: String
  , renderedText :: String
  }

_notebook :: Lens' AppState Notebook
_notebook = lens _.notebook ( _ { notebook = _ } )

_cells :: Lens' Notebook (Array Cell)
_cells = lens _.cells ( _ { cells = _ } )

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
