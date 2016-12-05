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
import WebSocket
import Data.Either
import Editor.CodeMirror
import Data.Tuple
import Data.String as S
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Var (($=))
import Data.Lens.Traversal (traversed)
import Pux (noEffects)
import Signal.Channel (CHANNEL, send, Channel)
import Data.Argonaut hiding ((:=))

initialNotebook :: Notebook
initialNotebook = Notebook
  { title: ""
  , subtitle: ""
  , author: ""
  , date: ""
  , cells: [] :: Array Cell
  }

decodeReceived :: String -> Json
decodeReceived s = case jsonParser s of
    Right j -> j
    Left _ -> fromString ""

initialAppState :: Channel Action -> String -> forall e. Eff (err::EXCEPTION, ws::WEBSOCKET|e) AppState
initialAppState chan url = do
    connection@(Connection ws) <- newWebSocket (URL url) []
    ws.onopen $= \event -> do
        ws.send (Message "Hi! I am client")
    ws.onmessage $= \event -> do
        let received = runMessage (runMessageEvent event)
        let nb = decodeJson (decodeReceived received) :: Either String Notebook
        case nb of
            Left s ->
                send chan (NoOp :: Action)
            Right n ->
                send chan ((UpdateNotebook n) :: Action)
    pure $ AppState
        { editing: true
        , notebook: initialNotebook
        , totalCells: 0
        , currentCell: 0
        , activeChannel : chan
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
    emptyCodeCell = Cell { cellId : (getTotalCells as), cellContent: "-- Type here", cellType: CodeCell }

addDisplayCell :: String -> AppState -> AppState
addDisplayCell msg as = appendCell displayCell as
  where
    displayCell = Cell { cellId : (getTotalCells as), cellContent: msg, cellType: DisplayCell }

getTotalCells :: AppState -> Int
getTotalCells = view _totalCells

countCellsInNotebook :: AppState -> Int
countCellsInNotebook =
    fromMaybe 0 <<< maximumOf (_notebook <<< _cells <<< traversed <<< _cellId)

updateNotebook :: Notebook -> AppState -> AppState
updateNotebook n (AppState as) =
    (_totalCells .~ countCellsInNotebook updatedNotebook + 1) updatedNotebook
  where
    updatedNotebook = AppState $ as { notebook = n }

updateCell :: Int -> String -> AppState -> AppState
updateCell i s =
  over (_notebook <<< _cells) (\x -> map updateCell' x)
  where
    isCorrectCell (Cell c) = c.cellId == i
    updateCell' (Cell c) = if isCorrectCell (Cell c) then Cell c { cellContent = s } else Cell c

update :: Action -> AppState -> EffModel AppState Action (ws :: WEBSOCKET, codemirror :: CODEMIRROR)
update ToggleEdit appState  = noEffects $ appState
update AddTextCell appState = noEffects $ addTextCell appState
update AddCodeCell appState =
    { state: addCodeCell appState
    , effects : [ pure $ RenderCodeCell (getTotalCells appState) ]
    }
update (RenderCodeCell i) appState@(AppState as) =
  { state: appState
  , effects: [ do liftEff $ makeCodeEditor as.activeChannel i ]
  }
update (CheckInput i ev) appState = noEffects $ updateCell i ev.target.value appState
update (CheckCode i s) appState = noEffects $ updateCell i s appState
update CheckNotebook as@(AppState appState) =
    { state: as
    , effects: [ do
        liftEff $ checkNotebook appState.socket appState.notebook
      ]
    }
update (UpdateNotebook receivedNotebook) appState = noEffects $ updateNotebook receivedNotebook appState
update NoOp appState = noEffects $ appState

makeCodeEditor :: ∀ eff . Channel Action -> Int -> Eff ( channel :: CHANNEL, codemirror :: CODEMIRROR | eff ) Action
makeCodeEditor chan i = do
    editor <- liftEff $ fromTextArea (show i) [ "mode" := "haskell" ]
    nextAction :: Action <- onChange editor (\code -> CheckCode i code)
    send chan nextAction
    pure NoOp

checkNotebook :: forall eff . Connection -> Notebook -> Eff ( ws :: WEBSOCKET, err :: EXCEPTION | eff ) Action
checkNotebook (Connection ws) n = do
    let s = encodeJson n
    ws.send (Message $ show s) *> pure NoOp
