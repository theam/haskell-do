module Types where

import Data.Lens
import Pux.Html.Events
import Control.Monad.Aff
import Signal.Channel
import Control.Monad.Eff.Exception (EXCEPTION)

data Action
  = ToggleEdit
  | AddTextCell
  | AddCodeCell
  | RenderCodeCell Int
  | CheckInput Int FormEvent
  | NoOp

type AppState =
  { editing :: Boolean
  , notebook :: Notebook
  , rawText :: String
  , renderedText :: String
  , totalCells :: Int
  , currentCell :: Int
  }

type EffModel state action eff =
  { state :: state
  , effects :: Array (Aff (channel :: CHANNEL, err :: EXCEPTION | eff) action)
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
  = TextCell Int String
  | CodeCell Int String DisplayResult

newtype DisplayResult = DisplayResult String
