module Cells.Types where

import Data.Show (show)
import Prelude (class Eq, class Show, ($), (/=), (<<<), (<>))

import Data.Maybe (fromMaybe)
import Data.Generic (class Generic)
import Signal.Channel (Channel)
import Data.Array (dropWhile, filter, length, tail, takeWhile)
import Data.Argonaut (class DecodeJson, class EncodeJson, gDecodeJson, gEncodeJson)

defaultCellText :: String
defaultCellText = "Type here"

data Action
    = AddCodeCell
    | AddTextCell
    | SaveContent    CellId String
    | RemoveCell     CellId
    | RenderTextCell CellId
    | RenderCodeCell CellId
    | NoOp

type State = 
    { currentCell :: CellId
    , cells       :: Array Cell
    , editorChanges :: Channel Action
    }

-- | Inserts a cell after a given Cell Id.
--   Cells could not be ordered:
--   > insertAfter 2 (newTextCell 4) s == s'
--   `s` is a State with the cells ordered as [2, 0, 1]
--   `s'` would be the same but with the order [2, 4, 0, 1]
insertAfter :: CellId -> Cell -> State -> State
insertAfter cId c s = s { cells = firstHalf <> [c] <> secondHalf }
  where
    firstHalf = takeWhile cellIdIsNotTheWanted s.cells
    secondHalf = fromMaybe [] <<< tail $ dropWhile cellIdIsNotTheWanted s.cells
    cellIdIsNotTheWanted (Cell incomingCell) = incomingCell.cellId /= cId

removeCell :: CellId -> State -> State
removeCell cId s = s { cells = removedCell }
  where
    removedCell = filter (\(Cell cell) -> cell.cellId /= cId) s.cells

totalCells :: State -> Int
totalCells s = length s.cells

newtype CellId = CellId Int

derive instance genericCellId :: Generic CellId
derive instance eqCellId :: Eq CellId
instance showCellId :: Show CellId where
    show (CellId i) = show i

data Cell = Cell
    { cellType    :: CellType
    , cellId      :: CellId
    , cellContent :: String
    }

derive instance genericCell :: Generic Cell

instance encodeJsonCell :: EncodeJson Cell where
    encodeJson = gEncodeJson

instance decodeJsonCell :: DecodeJson Cell where
    decodeJson = gDecodeJson

newCell :: CellType -> String -> CellId -> Cell
newCell cType cContent id' = Cell
    { cellId      : id'
    , cellContent : cContent
    , cellType    : cType
    }

newTextCell :: CellId -> Cell
newTextCell = newCell TextCell defaultCellText

newCodeCell :: CellId -> Cell
newCodeCell = newCell CodeCell ("-- " <> defaultCellText)

data CellType
    = TextCell
    | CodeCell

derive instance genericCellType :: Generic CellType

instance encodeJsonCellType :: EncodeJson CellType where
    encodeJson = gEncodeJson

instance decodeJsonCellType :: DecodeJson CellType where
    decodeJson = gDecodeJson
