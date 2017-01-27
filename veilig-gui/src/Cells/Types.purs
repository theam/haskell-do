module Cells.Types where

import Prelude
import Data.Argonaut
import Data.Argonaut.Decode.Class (gDecodeJson)
import Data.Array (dropWhile, filter, length, tail, takeWhile, foldr, (:))
import Data.Either (Either(Left))
import Data.Generic (class Generic)
import Data.Maybe (fromMaybe)
import Data.Show (show)
import Signal.Channel (Channel)

defaultCellText :: String
defaultCellText = "Type here"

data Action
    = AddCodeCell
    | AddTextCell
    | SaveContent    CellId String
    | RemoveCell     CellId
    | RenderTextCell CellId
    | RenderCodeCell CellId
    | SetCurrentCell CellId
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
    firstHalf = takeWhileOneMore cellIdIsNotTheWanted s.cells
    secondHalf = fromMaybe [] <<< tail $ dropWhile cellIdIsNotTheWanted s.cells
    cellIdIsNotTheWanted (Cell incomingCell) = incomingCell.cellId /= cId
    takeWhileOneMore p = foldr (\x ys -> if p x then x:ys else [x]) []

insertAtEnd :: Cell -> State -> State
insertAtEnd c s = s { cells = s.cells <> [c] }

removeCell :: CellId -> State -> State
removeCell cId s = s { cells = removedCell, currentCell = 0 }
  where
    removedCell = filter (\(Cell cell) -> cell.cellId /= cId) s.cells

totalCells :: State -> Int
totalCells s = length s.cells

type CellId = Int

newtype Cell = Cell
    { cellType    :: CellType
    , cellId      :: CellId
    , cellContent :: String
    }

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

instance encodeJsonCellType :: EncodeJson CellType where
    encodeJson TextCell = fromString "TextCell"
    encodeJson CodeCell = fromString "CodeCell"

instance decodeJsonCellType :: DecodeJson CellType where
    decodeJson json = do
      s <- decodeJson json
      case s of
        "TextCell"    -> pure TextCell
        "CodeCell"    -> pure CodeCell
        _             -> Left "Could not decode display Cell"
