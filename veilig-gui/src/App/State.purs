module App.State where

import Prelude

import Cells.State   as Cells
import Cells.Types   as Cells
import Columns.State as Columns
import Columns.Types as Columns
import App.Types     as App
import Notebook.Types
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
  , cells: [] :: Array Cells.Cell
  , console: ">"
  }

decodeReceived :: String -> Json
decodeReceived s = case jsonParser s of
    Right j -> j
    Left _ -> fromString ""

initialAppState :: ∀ e. Channel App.Action -> String -> Eff (err::EXCEPTION, ws::WEBSOCKET|e) App.State
initialAppState chan url = do
    connection@(Connection ws) <- newWebSocket (URL url) []
    ws.onopen $= \event -> do
        ws.send (Message "HaskellDO:Client")
    ws.onmessage $= \event -> do
        let received = runMessage (runMessageEvent event)
        let nb = decodeJson (decodeReceived received) :: Either String Notebook
        case nb of
            Left s ->
                send chan (NoOp :: App.Action)
            Right n ->
                send chan ((UpdateNotebook n) :: App.Action)
    pure $ AppState
        { notebook: initialNotebook
        , activeChannel : chan
        , socket : connection
        }

updateNotebook :: Notebook -> AppState -> AppState
updateNotebook n (AppState as) =
    (_totalCells .~ countCellsInNotebook updatedNotebook + 1) updatedNotebook
  where
    updatedNotebook = AppState $ as { notebook = n }

update :: Action -> AppState -> EffModel AppState Action (ws :: WEBSOCKET, codemirror :: CODEMIRROR)
update (CellAction action) as = Cells.update action (view cellsState as)
update (ColumnsAction action) as = Columns.update action (view columnsState as)
update (ConsoleAction action) as = ConsoleAction.update action (view consoleState as)
update (BackendConnectionAction action) as = BackendConnection.update action (view backendConnectionState as)


update (UpdateNotebook receivedNotebook) appState = noEffects $ updateNotebook receivedNotebook appState
update NoOp appState = noEffects $ appState

checkNotebook :: ∀ eff . Connection -> Notebook -> Eff ( ws :: WEBSOCKET, err :: EXCEPTION | eff ) Action
checkNotebook (Connection ws) n = do
    let s = encodeJson n
    ws.send (Message $ show s) *> pure NoOp

addToConsole :: String -> AppState -> AppState
addToConsole consoleText (AppState as) =
    AppState $ as { consoleBuffer = consoleText }
  where
    input = Str.drop 1 $ Str.dropWhile (\c -> c /= '>') consoleText