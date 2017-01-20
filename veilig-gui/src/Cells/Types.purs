module Cells.Types
  ( Action(..)
  , State
  , EffModel
  , CellId
  , Cell
  , CellType(..)
  )
where

import Prelude

import Data.Generic

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

type EffModel eff =
    { state :: State
    , effects :: Array ( Aff ( channel :: CHANNEL | eff ) Action )
    }

data State = State
    { _currentCell :: CellId
    , _cells       :: Array Cell
    , _editorChanges :: Channel Action
    }

-- | Inserts a cell after a given Cell Id.
--   Cells could not be ordered:
--   > insertAfter 2 (newTextCell 4) s == s'
--   `s` is a State with the cells ordered as [2, 0, 1]
--   `s'` would be the same but with the order [2, 4, 0, 1]
insertAfter :: CellId -> Cell -> State -> State
insertAfter cId c s = (cells .~ firstHalf <> [c] <> secondHalf) s
  where
    firstHalf = takeWhile cellIdIsNotTheWanted $ view cells s
    secondHalf = tail . dropWhile cellIdIsNotTheWanted $ view cells s
    cellIdIsNotTheWanted incomingCell = view cellId incomingCell /= cId

removeCell :: CellId -> State -> State
removeCell cId s = over cells removeCellFromArray
  where
    removeCellFromArray = filter (\cell -> view cellId cell /= cId)

totalCells :: State -> Int
totalCells = length . view cells

newtype CellId = CellId Int

derive instance genericState :: Generic State

data Cell = Cell
    { _cellType    :: CellType
    , _cellId      :: CellId
    , _cellContent :: String
    }

derive instance genericCell :: Generic Cell

instance encodeJsonCell :: EncodeJson Cell where
    encodeJson = gEncodeJson

instance decodeJsonCell :: DecodeJson Cell where
    decodeJson = gDecodeJson

newCell :: CellType -> String -> CellId -> Cell
newCell cType cContent id' = Cell
    { _cellId      : id'
    , _cellContent : cContent
    , _cellType    : cType
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

-------------------------------------------------------------------
-- Lenses declarations, with template Haskell this would go away --
-- but in PureScript we must do it manually.                     --
-------------------------------------------------------------------

currentCell :: Lens' State Int
currentCell = lens
    (\(State s) -> s._currentCell)
    (\(State s) -> (\newValue -> State (s { _currentCell = newValue})))

cells :: Lens' State (Array Cell)
cells = lens
    (\(State s) -> s._cells)
    (\(State s) -> (\newValue -> State (s { _cells = newValue})))

editorChanges :: Lens' State Int
editorChanges = lens
    (\(State s) -> s._editorChanges)
    (\(State s) -> (\newValue -> State (s { _editorChanges = newValue})))
    
cellId :: Lens' Cell Int
cellId = lens
    (\(Cell c) -> c._cellId)
    (\(Cell c) -> (\newValue -> Cell (c { _cellId = newValue})))

cellType :: Lens' Cell CellType
cellType = lens
    (\(Cell c) -> c._cellType)
    (\(Cell c) -> (\newValue -> Cell (c { _cellType = newValue})))

cellContent :: Lens' Cell CellType
cellContent = lens
    (\(Cell c) -> c._cellContent)
    (\(Cell c) -> (\newValue -> Cell (c { _cellContent = newValue})))
