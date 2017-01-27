module App.State where

import Prelude


import Cells.Types as Cells
import Cells.State (update) as Cells
import Columns.State as Columns
import Columns.Types as Columns
import Console.State as Console
import Console.Types as Console
import BackendConnection.State as BackendConnection
import BackendConnection.Types as BackendConnection
import App.Types
import Notebook.Types
import Notebook.Packer as Notebook

import Pux
import Global.Effects

initialState :: Cells.State -> Columns.State -> BackendConnection.State Notebook -> Console.State -> State
initialState cellsState columnsState backendConnectionState consoleState =
    { cellsState : cellsState
    , columnsState : {}
    , backendConnectionState : backendConnectionState
    , consoleState : consoleState
    }


update :: Action -> State -> EffModel State Action GlobalEffects
update (CellsAction action) state =
  Cells.update action state.cellsState
  # mapState (state { cellsState = _ })
  # mapEffects CellsAction

update (ColumnsAction action) state =
  Columns.update action state.columnsState
  # mapState (state { columnsState = _ })
  # mapEffects ColumnsAction

update (ConsoleAction action) state =
  Console.update action state.consoleState
  # mapState (state { consoleState = _ })
  # mapEffects ConsoleAction

update (BackendConnectionAction action) state =
  BackendConnection.update action state.backendConnectionState
  # mapState (state { backendConnectionState = _ })
  # mapEffects BackendConnectionAction

update BuildAndSend state = onlyEffects state [ do
    let notebook = Notebook.pack state.cellsState state.consoleState
    pure $ BackendConnectionAction (BackendConnection.Send notebook)
  ]

update NoOp appState = noEffects $ appState
