module State where

import Prelude
import Types

initialAppState :: AppState
initialAppState =
  { editing: true
  , rawText: ""
  , renderedText: ""
  }

update :: Action -> AppState -> AppState
update ToggleEdit state = state { editing = not state.editing }
