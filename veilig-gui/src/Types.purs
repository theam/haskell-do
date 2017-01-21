module Types where

import Prelude
import Data.Argonaut
import Data.Ord
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Either (Either(Left))
import Data.Lens (Lens', lens)
import Data.Show (class Show)
import Prelude (class Eq, pure, bind, ($))
import Pux.Html.Events (FormEvent)
import Signal.Channel (Channel, CHANNEL)
import WebSocket (Connection)
import Cells.Types as Cells
import Columns.Types as Columns
import Console.Types as Console

data Action
    = CellsAction Cells.Action
    | ColumnsAction Columns.Action
    | ConsoleAction Console.Action
    | CheckCode Int String
    | CheckNotebook
    | UpdateNotebook Notebook
    | NoOp

newtype AppState = AppState
    { _cellsState    :: Cells.State
    , _columnsState  :: Columns.State
    , _consoleState  :: Console.State
    , activeChannel :: Channel Action
    , socket        :: Connection
    , consoleBuffer :: String
    }

type EffModel state action eff =
    { state :: state
    , effects :: Array (Aff (channel :: CHANNEL, err :: EXCEPTION | eff) action)
    }

_consoleBuffer :: Lens' AppState String
_consoleBuffer = lens
    (\(AppState s) -> s.consoleBuffer)
    (\(AppState s) -> (\n -> AppState (s { consoleBuffer = n})))

_socket :: Lens' AppState Connection
_socket = lens
    (\(AppState s) -> s.socket)
    (\(AppState s) -> (\n -> AppState (s { socket = n})))

data Notebook = Notebook
    { title :: String
    , subtitle :: String
    , date :: String
    , author :: String
    , cells :: Array Cells.Cell
    , console :: String
    }

derive instance genericNotebook :: Generic Notebook

instance encodeJsonNotebook :: EncodeJson Notebook where
    encodeJson = gEncodeNotebook

instance decodeJsonNotebook :: DecodeJson Notebook where
    decodeJson = gDecodeNotebook

_console :: Lens' Notebook String
_console = lens
    (\(Notebook n) -> n.console)
    (\(Notebook n) -> (\c -> Notebook (n { console = c})))

