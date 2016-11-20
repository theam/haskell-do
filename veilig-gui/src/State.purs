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
import WebSocket
import Data.String as S
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Lens.Traversal (traversed)
import Pux (noEffects)
import Signal.Channel (send, Channel)
import Control.Monad.Eff.Var (($=))

initialNotebook :: Notebook
initialNotebook = Notebook
  { title: ""
  , subtitle: ""
  , author: ""
  , date: ""
  , cells: [] :: Array Cell
  }

initialAppState :: Channel Action -> String -> forall e. Eff (err::EXCEPTION, ws::WEBSOCKET|e) AppState
initialAppState chan url = do
    connection@(Connection ws) <- newWebSocket (URL url) []
    ws.onopen $= \event -> do
        ws.send (Message "Hi! I am client")
    ws.onmessage $= \event -> do
        let received = runMessage (runMessageEvent event)
        send chan ((DisplayMessage received) :: Action)
    pure $ AppState
        { editing: true
        , notebook: initialNotebook
        , totalCells: 0
        , currentCell: 0
        , socket : connection
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

addDisplayCell :: String -> AppState -> AppState
addDisplayCell msg as = appendCell displayCell as
  where
    displayCell = Cell { cellId : (getTotalCells as), cellContent: msg, cellType: DisplayCell }

getTotalCells = view _totalCells


updateCell :: Int -> String -> AppState -> AppState
updateCell i s =
  over (_notebook <<< _cells) (\x -> map updateCell' x)
  where
    isCorrectCell (Cell c) = c.cellId == i
    updateCell' (Cell c) = if isCorrectCell (Cell c) then Cell c { cellContent = s } else Cell c

update :: Action -> AppState -> EffModel AppState Action (ws :: WEBSOCKET)
update ToggleEdit appState  = noEffects $ appState
update AddTextCell appState = noEffects $ addTextCell appState
update AddCodeCell appState = noEffects $ addCodeCell appState
update (RenderCodeCell i) appState =
  { state: appState
  , effects: [ do
      pure NoOp
    ]
  }
update (CheckInput i ev) appState = noEffects $ updateCell i ev.target.value appState
update CheckNotebook as@(AppState appState) =
    { state: as
    , effects: [ do
        liftEff $ checkNotebook appState.socket appState.notebook
      ]
    }
update (DisplayMessage received) appState = noEffects $ addDisplayCell received appState
update NoOp appState = noEffects $ appState

checkNotebook :: forall eff . Connection -> Notebook -> Eff ( ws :: WEBSOCKET, err :: EXCEPTION | eff ) Action
checkNotebook (Connection ws) n = do
    let s = encodeJson n
    ws.send (Message $ show s) *> pure NoOp
