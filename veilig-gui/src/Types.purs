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
import Navbar.Actions

data Action
    = NavbarAction NavbarAction
    | AppendConsole String
    | SendConsole
    | RenderCodeCell Int
    | CheckInput Int FormEvent
    | CheckCode Int String
    | CheckNotebook
    | UpdateNotebook Notebook
    | RenderTextCell Int
    | AddToConsole String
    | NoOp

newtype AppState = AppState
    { notebook :: Notebook
    , totalCells :: Int
    , currentCell :: Int
    , activeChannel :: Channel Action
    , socket :: Connection
    , consoleBuffer :: String
    }

type EffModel state action eff =
    { state :: state
    , effects :: Array (Aff (channel :: CHANNEL, err :: EXCEPTION | eff) action)
    }

_notebook :: Lens' AppState Notebook
_notebook = lens
    (\(AppState s) -> s.notebook)
    (\(AppState s) -> (\n -> AppState (s { notebook = n})))

_totalCells :: Lens' AppState Int
_totalCells = lens
    (\(AppState s) -> s.totalCells)
    (\(AppState s) -> (\n -> AppState (s { totalCells = n})))

_currentCell :: Lens' AppState Int
_currentCell = lens
    (\(AppState s) -> s.currentCell)
    (\(AppState s) -> (\c -> AppState (s { currentCell = c})))

_consoleBuffer :: Lens' AppState String
_consoleBuffer = lens
    (\(AppState s) -> s.consoleBuffer)
    (\(AppState s) -> (\n -> AppState (s { consoleBuffer = n})))

_socket :: Lens' AppState Connection
_socket = lens
    (\(AppState s) -> s.socket)
    (\(AppState s) -> (\n -> AppState (s { socket = n})))

newtype Notebook = Notebook
    { title :: String
    , subtitle :: String
    , date :: String
    , author :: String
    , cells :: Array Cell
    , console :: String
    }

instance encodeJsonNotebook :: EncodeJson Notebook where
    encodeJson (Notebook n)
        = "title" := n.title
       ~> "subtitle" := n.subtitle
       ~> "date" := n.date
       ~> "author" := n.author
       ~> "cells" := n.cells
       ~> "console" := n.console
       ~> jsonEmptyObject

instance decodeJsonNotebook :: DecodeJson Notebook where
    decodeJson json = do
        o <- decodeJson json
        title <- o .? "title"
        subtitle <- o .? "subtitle"
        date <- o .? "date"
        author <- o .? "author"
        cells <- o.? "cells"
        console <- o.? "console"
        pure $ Notebook {title, subtitle, date, author, cells, console}

_cells :: Lens' Notebook (Array Cell)
_cells = lens
    (\(Notebook n) -> n.cells)
    (\(Notebook n) -> (\c -> Notebook (n { cells = c})))

_console :: Lens' Notebook String
_console = lens
    (\(Notebook n) -> n.console)
    (\(Notebook n) -> (\c -> Notebook (n { console = c})))

newtype Cell = Cell
    { cellType :: CellType
    , cellId :: Int
    , cellContent :: String
    }

_cellId :: Lens' Cell Int
_cellId = lens
    (\(Cell n) -> n.cellId)
    (\(Cell n) -> (\c -> Cell (n { cellId = c})))

instance ordCell :: Ord Cell where
    compare (Cell c1) (Cell c2) = c1.cellId `compare` c2.cellId

instance eqCell :: Eq Cell where
    eq (Cell c1) (Cell c2) = c1.cellId == c2.cellId

data CellType
    = TextCell
    | CodeCell
    | DisplayCell

instance showCellType :: Show CellType where
    show TextCell = "TextCell"
    show CodeCell = "CodeCell"
    show DisplayCell = "DisplayCell"

instance encodeJsonCellType :: EncodeJson CellType where
    encodeJson TextCell = fromString "TextCell"
    encodeJson CodeCell = fromString "CodeCell"
    encodeJson DisplayCell = fromString "DisplayCell"

instance decodeJsonCellType :: DecodeJson CellType where
    decodeJson json = do
      s <- decodeJson json
      case s of
        "TextCell"    -> pure TextCell
        "CodeCell"    -> pure CodeCell
        "DisplayCell" -> pure DisplayCell
        _             -> Left "Could not decode display Cell"

instance encodeJsonCell :: EncodeJson Cell where
    encodeJson (Cell c)
        = "cellId" := c.cellId
       ~> "cellType" := c.cellType
       ~> "cellContent" := c.cellContent
       ~> jsonEmptyObject

instance decodeJsonCell :: DecodeJson Cell where
    decodeJson json = do
        o <- decodeJson json
        cellType <- o .? "cellType"
        cellId <- o .? "cellId"
        cellContent <- o .? "cellContent"
        pure $ Cell { cellType, cellId, cellContent }

newtype DisplayResult = DisplayResult String
