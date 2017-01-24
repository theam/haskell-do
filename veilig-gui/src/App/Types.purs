module App.Types where

import Data.Lens
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Signal.Channel (CHANNEL)
import Cells.Types as Cells
import Columns.Types as Columns
import Console.Types as Console
import BackendConnection.Types as BackendConnection
import Notebook.Types (Notebook)

data Action
    = CellsAction Cells.Action
    | ColumnsAction Columns.Action
    | ConsoleAction Console.Action
    | BackendConnectionAction (BackendConnection.Action Notebook)
    | CheckNotebook
    | NoOp

type State = 
    { cellsState              :: Cells.State
    , columnsState            :: Columns.State
    , consoleState            :: Console.State
    , backendConnectionState  :: BackendConnection.State Notebook
    }
