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
  over (_notebook <<< _cells) (\x -> map updateCell' x)
  where
    isCorrectCell (Cell c) = c.cellId == i
    updateCell' (Cell c) = if isCorrectCell (Cell c) then Cell c { cellContent = s } else Cell c

update :: Action -> AppState -> EffModel AppState Action (makeEditor :: MAKEEDITOR, websocket :: WEBSOCKET)
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
update CheckNotebook as@(AppState appState) =
    { state: as
    , effects: [ do
        liftEff $ checkNotebook appState.notebook
        pure NoOp
      ]
    }
update NoOp appState = noEffects $ appState

checkNotebook :: forall eff . Notebook -> Eff ( websocket :: WEBSOCKET | eff ) Action
checkNotebook n = checkNotebookImpl $ encodeJson n


foreign import data MAKEEDITOR :: !
foreign import makeEditor :: forall eff. Int -> Eff ( makeEditor :: MAKEEDITOR | eff ) Action

foreign import data WEBSOCKET :: !
foreign import checkNotebookImpl :: forall eff. Json -> Eff ( websocket :: WEBSOCKET | eff ) Action
