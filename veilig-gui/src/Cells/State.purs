module Cells.State (update, initialState) where

import Prelude

import Cells.Types

initialState :: State
initialState = State
    { _currentCell : 0  :: Int
    , _cells       : [] :: Array Cell
    }

addCodeCell :: State -> State
addCodeCell s = insertAfter (view currentCell s) (newCodeCell $ totalCells state) s

addTextCell :: State -> State
addTextCell s = insertAfter (view currentCell s) (newTextCell $ totalCells state) s

saveContent :: CellId -> String -> State -> State
saveContent cId newContent = over cells (\cell' -> map updateCell cell')
  where
    isCorrectCell c = view cellId c == cId
    updateCell c =
        if isCorrectCell c
            then (cellContent .~ newContent) c
            else c

update :: Action -> State -> EffModel ( dom :: DOM )
update AddCodeCell s = 
    { state : addCodeCell s
    , effects : [ pure $ RenderCodeCell ( CellId $ totalCells s ) ]
    }

update AddTextCell s = 
    { state : addTextCell s
    , effects : [ pure $ RenderTextCell ( CellId $ totalCells s ) ]
    }

update (RenderCodeCell $ CellId i) s  =
    { state : s
    , effects : [ liftEff $ makeCodeEditor (view editorChanges s) i ]
    }

update (RenderTextCell $ CellId i) s  =
    { state : s
    , effects : [ liftEff $ makeTextEditor (view editorChanges s) i ]
    }

update (SaveContent cId newContent) s = noEffects $ saveContent cId newContent s

update (RemoveCell cId) s             = noEffects $ removeCell  cId s

update NoOp s                         = noEffects $ s