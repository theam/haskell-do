module State where

import Prelude
import Types

initialNotebook :: Notebook
initialNotebook =
  { title: ""
  , subtitle: ""
  , author: ""
  , date: ""
  , cells: [ TextCell "Type here" ] :: Array Cell
  }

initialAppState :: AppState
initialAppState =
  { editing: true
  , notebook: initialNotebook
  , rawText: ""
  , renderedText: ""
  }

update :: Action -> AppState -> AppState
update ToggleEdit state = state { editing = not state.editing }
  
