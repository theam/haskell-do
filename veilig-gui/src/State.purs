module State where

import Prelude
import Types
import Data.Array
import Data.Lens
import Data.Lens.Index
import Data.Lens.Setter
import Data.Array
import Data.Maybe
import Control.Monad.Aff
import Control.Monad.Eff
import Data.String as S
import Control.Monad.Eff.Class (liftEff)
import Data.Lens.Traversal (traversed)
import Pux (noEffects)

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
  , totalCells: 0
  , currentCell: 0
  }

appendCell :: Cell -> AppState -> AppState
appendCell c appState =
  (_notebook <<< _cells ) <>~ [c]
    $ appState
        { totalCells = appState.totalCells + 1
        }

addTextCell :: AppState -> AppState
addTextCell as = appendCell (TextCell as.totalCells "Type here") as

addCodeCell :: AppState -> AppState
addCodeCell as = appendCell emptyCodeCell as
  where
    emptyCodeCell = CodeCell as.totalCells "Code" (DisplayResult "")



updateCell :: Int -> String -> AppState -> AppState
updateCell i s =
  over (_notebook <<< _cells <<< traversed <<< filtered (isCorrectCell i)) updateCell'
  where
    isCorrectCell i (TextCell i' _) = i == i'
    isCorrectCell i (CodeCell i' _ _) = i == i'
    updateCell' (TextCell i' s') = TextCell i' s
    updateCell' (CodeCell i' s' d) = CodeCell i' s d

update :: Action -> AppState -> EffModel AppState Action (makeEditor :: MAKEEDITOR)
update ToggleEdit appState  = noEffects $ appState { editing = not appState.editing }
update AddTextCell appState = noEffects $ addTextCell appState
update AddCodeCell appState = noEffects $ addCodeCell appState
update (RenderCodeCell i) appState =
  { state: appState { editing = not appState.editing }
  , effects: [ do
      liftEff $ makeEditor i
      pure NoOp
    ]
  }
update (CheckInput i ev) appState = noEffects $ updateCell i ev.target.value appState
update NoOp appState = noEffects $ appState


foreign import data MAKEEDITOR :: !

foreign import makeEditor :: forall eff. Int -> Eff ( makeEditor :: MAKEEDITOR | eff ) Action
