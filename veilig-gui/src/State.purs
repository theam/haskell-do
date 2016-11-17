module State where

import Prelude
import Types
import Data.Array
import Data.Lens
import Data.Lens.Index
import Data.Lens.Setter
import Data.Array
import Data.Maybe
import Data.Argonaut
import Control.Monad.Aff
import Control.Monad.Eff
import Data.String as S
import Control.Monad.Eff.Class (liftEff)
import Data.Lens.Traversal (traversed)
import Pux (noEffects)

initialNotebook :: Notebook
initialNotebook = Notebook
  { title: ""
  , subtitle: ""
  , author: ""
  , date: ""
  , cells: [] :: Array Cell
  }

initialAppState :: AppState
initialAppState = AppState
  { editing: true
  , notebook: initialNotebook
  , totalCells: 0
  , currentCell: 0
  }

appendCell :: Cell -> AppState -> AppState
appendCell c =
  ((_notebook <<< _cells ) <>~ [c]) <<< (_totalCells +~ 1)

addTextCell :: AppState -> AppState
addTextCell as = appendCell (Cell { cellId : (getTotalCells as), cellContent: "Type here", cellType: TextCell} ) as

addCodeCell :: AppState -> AppState
addCodeCell as = appendCell emptyCodeCell as
  where
    emptyCodeCell = Cell { cellId : (getTotalCells as), cellContent: "Type here", cellType: CodeCell }

getTotalCells = view _totalCells


updateCell :: Int -> String -> AppState -> AppState
updateCell i s =
  over (_notebook <<< _cells <<< traversed <<< filtered (isCorrectCell i)) updateCell'
  where
    isCorrectCell i (Cell c) = c.cellId == i
    updateCell' (Cell c) = Cell c { cellContent = s }

update :: Action -> AppState -> EffModel AppState Action (makeEditor :: MAKEEDITOR)
update ToggleEdit appState  = noEffects $ appState
update AddTextCell appState = noEffects $ addTextCell appState
update AddCodeCell appState = noEffects $ addCodeCell appState
update (RenderCodeCell i) appState =
  { state: appState
  , effects: [ do
      liftEff $ makeEditor i
      pure NoOp
    ]
  }
update (CheckInput i ev) appState = noEffects $ updateCell i ev.target.value appState
update NoOp appState = noEffects $ appState

cn :: forall eff . Notebook -> Eff ( websocket :: WEBSOCKET | eff ) Action
cn n = checkNotebookImpl $ encodeJson n


foreign import data MAKEEDITOR :: !
foreign import makeEditor :: forall eff. Int -> Eff ( makeEditor :: MAKEEDITOR | eff ) Action

foreign import data WEBSOCKET :: !
foreign import checkNotebookImpl :: forall eff. Json -> Eff ( websocket :: WEBSOCKET | eff ) Action
