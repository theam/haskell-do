module Main where

import Prelude

import Notebook.Types (Notebook)
import App.Types
import App.State as App
import App.View as App
import BackendConnection.Types as BackendConnection
import BackendConnection.State as BackendConnection
import Cells.Types as Cells
import Cells.State as Cells
import Console.Types as Console
import Console.State as Console
import Columns.Types as Columns
import Columns.State as Columns

import WebSocket (URL(..))
import DOM
import Signal
import Control.Monad.Eff (Eff)
import Global.Effects (GlobalEffects)
import Pux (CoreEffects, renderToDOM, fromSimple, start)
import Signal.Channel (subscribe, channel, CHANNEL, Channel)

main :: Eff (CoreEffects GlobalEffects) Unit
main = do
    cellsChannel <- channel Cells.NoOp
    backendConnectionChannel <- channel (BackendConnection.NoOp :: BackendConnection.Action Notebook)
    consoleChannel <- channel Console.NoOp
    backendConnectionState <- BackendConnection.initialState backendConnectionChannel (URL "ws://127.0.0.1:3000")

    let cellsState             = Cells.initialState cellsChannel
        consoleState           = Console.initialState consoleChannel
        inputSignals = 
            [ consoleChannel `liftAction` buildAndSend
            , backendConnectionChannel `liftAction` updateStateAfterReceiving
            , cellsChannel `liftAction` CellsAction
            ]
    app <- start
        { initialState : App.initialState cellsState {} backendConnectionState consoleState
        , update : App.update
        , view : App.view
        , inputs : inputSignals
        }
    renderToDOM "#app" app.html

buildAndSend :: Console.Action -> Action
buildAndSend Console.PackAndSendToBackend = BuildAndSend
buildAndSend _ = NoOp

updateStateAfterReceiving :: BackendConnection.Action Notebook -> Action
updateStateAfterReceiving (BackendConnection.Receive n) = UpdateState n
updateStateAfterReceiving _ = NoOp

liftAction :: forall subaction action . Channel subaction -> (subaction -> action) -> Signal action
liftAction chan act = map act $ subscribe chan