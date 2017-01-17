module State where

import Prelude
import Types
import Data.Array
import Data.Lens
import Data.Lens.Index
import Data.Lens.Setter
import Data.Array
import Data.Maybe
import Data.String as Str
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
  , console: ">"
  }

decodeReceived :: String -> Json
decodeReceived s = case jsonParser s of
    Right j -> j
    Left _ -> fromString ""

initialAppState :: Channel Action -> String -> ∀ e. Eff (err::EXCEPTION, ws::WEBSOCKET|e) AppState
initialAppState chan url = do
    connection@(Connection ws) <- newWebSocket (URL url) []
    ws.onopen $= \event -> do
        ws.send (Message "HaskellDO:Client")
    ws.onmessage $= \event -> do
        let received = runMessage (runMessageEvent event)
        let nb = decodeJson (decodeReceived received) :: Either String Notebook
        case nb of
            Left s ->
                send chan (NoOp :: Action)
            Right n ->
                send chan ((UpdateNotebook n) :: Action)
    pure $ AppState
        { notebook: initialNotebook
        , totalCells: 0
        , currentCell: 0
        , activeChannel : chan
        , socket : connection
        , consoleBuffer : ""
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
update AddTextCell appState =
    { state : addTextCell appState
    , effects : [ pure $ RenderTextCell (getTotalCells appState) ]
    }
update AddCodeCell appState =
    { state: addCodeCell appState
    , effects : [ pure $ RenderCodeCell (getTotalCells appState) ]
    }
update (RenderCodeCell i) appState@(AppState as) =
  { state: appState
  , effects: [ do liftEff $ makeCodeEditor as.activeChannel i ]
  }
update (RenderTextCell i) appState@(AppState as) =
    { state: appState
    , effects : [ liftEff $ makeTextEditor as.activeChannel i ]
    }
update (CheckInput i ev) appState = noEffects $ updateCell i ev.target.value appState
update (CheckCode i s) appState = noEffects $ updateCell i s appState
update CheckNotebook as=
    { state: as
    , effects: [ do
        let as' = ((_notebook <<< _console) .~ (view _consoleBuffer as)) as
        liftEff $ checkNotebook (view _socket as') (view _notebook as')
      ]
    }
update (UpdateNotebook receivedNotebook) appState = noEffects $ updateNotebook receivedNotebook appState
update SendConsole appState = noEffects $ appState
update (AppendConsole key) appState = noEffects $ addToConsole (view _consoleBuffer appState <> key) appState
update (AddToConsole consoleText) appState = noEffects $ addToConsole consoleText appState
update NoOp appState = noEffects $ appState

makeCodeEditor :: ∀ eff . Channel Action -> Int -> Eff ( channel :: CHANNEL, codemirror :: CODEMIRROR | eff ) Action
makeCodeEditor chan i = do
    editor <- liftEff $ fromTextArea (show i) { mode : "haskell" }
    onChange editor chan (\code -> CheckCode i code)
    pure NoOp

makeTextEditor :: ∀ eff . Channel Action -> Int -> Eff (channel :: CHANNEL, codemirror :: CODEMIRROR | eff ) Action
makeTextEditor chan i = do
    editor <- fromTextAreaMarkdownEditor (show i)
    onChange editor.codemirror chan (\txt -> CheckCode i txt)
    pure NoOp

checkNotebook :: ∀ eff . Connection -> Notebook -> Eff ( ws :: WEBSOCKET, err :: EXCEPTION | eff ) Action
checkNotebook (Connection ws) n = do
    let s = encodeJson n
    ws.send (Message $ show s) *> pure NoOp

addToConsole :: String -> AppState -> AppState
addToConsole consoleText (AppState as) =
    AppState $ as { consoleBuffer = consoleText }
  where
    input = Str.drop 1 $ Str.dropWhile (\c -> c /= '>') consoleText