module State where

import Prelude

import Types
import Data.Array
import Data.Lens
import Data.Lens.Setter

initialNotebook :: Notebook
initialNotebook =
  { title: ""
  , subtitle: ""
  , author: ""
  , date: ""
  , cells: [] :: Array Cell
  }

initialAppState :: AppState
initialAppState =
  { editing: true
  , notebook: initialNotebook
  , rawText: ""
  , renderedText: ""
  }

appendCell :: Cell -> AppState -> AppState
appendCell c =
  over (_notebook <<< _cells ) $ (<>) [c]

addTextCell :: AppState -> AppState
addTextCell appState = appendCell (TextCell "Type here") appState

update :: Action -> AppState -> AppState
update ToggleEdit state = state { editing = not state.editing }
  
