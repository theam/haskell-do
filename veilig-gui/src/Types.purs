module Types where

import Data.Lens
import Pux.Html.Events

data Action
  = ToggleEdit
  | AddTextCell
  | AddCodeCell
  | CheckInput FormEvent

type AppState =
  { editing :: Boolean
  , notebook :: Notebook
  , rawText :: String
  , renderedText :: String
  , currentCell :: Int
  }

_notebook :: Lens' AppState Notebook
_notebook = lens _.notebook ( _ { notebook = _ } )

_currentCell :: Lens' AppState Int
_currentCell = lens _.currentCell ( _ { currentCell = _ } )

type Notebook =
  { title :: String
  , subtitle :: String
  , date :: String
  , author :: String
  , cells :: Array Cell
  }

_cells :: Lens' Notebook (Array Cell)
_cells = lens _.cells ( _ { cells = _ } )

data Cell
  = TextCell String
  | CodeCell String DisplayResult

newtype DisplayResult = DisplayResult String
