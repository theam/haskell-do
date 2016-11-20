module Types where

import Data.Argonaut
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Either (Either(Left))
import Data.Lens (Lens', lens)
import Prelude (pure, bind, ($))
import Pux.Html.Events (FormEvent)
import Signal.Channel (CHANNEL)
import WebSocket (Connection)

data Action
    = ToggleEdit
    | AddTextCell
    | AddCodeCell
    | RenderCodeCell Int
    | CheckInput Int FormEvent
    | CheckNotebook
    | DisplayMessage String
    | NoOp

newtype AppState = AppState
    { editing :: Boolean
    , notebook :: Notebook
    , totalCells :: Int
    , currentCell :: Int
    , socket :: Connection
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

newtype Notebook = Notebook
    { title :: String
    , subtitle :: String
    , date :: String
    , author :: String
    , cells :: Array Cell
    }

instance encodeJsonNotebook :: EncodeJson Notebook where
    encodeJson (Notebook n)
        = "title" := n.title
       ~> "subtitle" := n.subtitle
       ~> "date" := n.date
       ~> "author" := n.author
       ~> "cells" := n.cells
       ~> jsonEmptyObject

instance decodeJsonNotebook :: DecodeJson Notebook where
    decodeJson json = do
        o <- decodeJson json
        title <- o .? "title"
        subtitle <- o .? "subtitle"
        date <- o .? "date"
        author <- o .? "author"
        cells <- o.? "cells"
        pure $ Notebook {title, subtitle, date, author, cells}

_cells :: Lens' Notebook (Array Cell)
_cells = lens
    (\(Notebook n) -> n.cells)
    (\(Notebook n) -> (\c -> Notebook (n { cells = c})))

newtype Cell = Cell
    { cellType :: CellType
    , cellId :: Int
    , cellContent :: String
    }

data CellType
    = TextCell
    | CodeCell
    | DisplayCell

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
