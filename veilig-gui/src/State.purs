module State where

import Prelude

import Types
import Data.Array
import Data.Lens
import Data.Lens.Index
import Data.Lens.Setter
import Data.String as S
import Data.Array
import Data.Maybe

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
  , currentCell: 0
  }

appendCell :: Cell -> AppState -> AppState
appendCell c = (_notebook <<< _cells ) <>~ [c]

addTextCell :: AppState -> AppState
addTextCell = appendCell (TextCell "Type here")

addCodeCell :: AppState -> AppState
addCodeCell = appendCell emptyCodeCell
  where
    emptyCodeCell = CodeCell "Code" (DisplayResult "")

update :: Action -> AppState -> AppState
update ToggleEdit appState  = appState { editing = not appState.editing }
update AddTextCell appState = addTextCell appState
update AddCodeCell appState = addCodeCell appState
update (CheckInput ev) appState = appState
