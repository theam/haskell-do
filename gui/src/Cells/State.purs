module Cells.State where

import Prelude
import Pux
import Cells.Types
import Cells.Foreign
import Data.Lens
import DOM
import Signal.Channel
import Control.Monad.Eff.Class (liftEff)
import Global.Effects
import Debug.Trace
import Data.Array
import Data.Maybe

initialState :: Channel Action -> State
initialState chan =
    { currentCell   : 0
    , cells         : [] :: Array Cell
    , editorChanges : chan
    }

addCodeCell :: State -> State
addCodeCell s = insertAtEnd (newCodeCell $ totalCells s) s

addTextCell :: State -> State
addTextCell s = insertAtEnd (newTextCell $ totalCells s) s

saveContent :: CellId -> String -> State -> State
saveContent cId newContent s = s { cells = map updateCell s.cells }
  where
    isCorrectCell (Cell c) = c.cellId == cId
    updateCell (Cell c) =
        if isCorrectCell (Cell c)
            then Cell c { cellContent = newContent }
            else Cell c

update :: Update State Action GlobalEffects
update AddCodeCell s =
    { state : newState
    , effects : [ pure $ RenderCodeCell ( totalCells s) ]
    }
  where
    newState = addCodeCell s

update AddTextCell s =
    { state : newState
    , effects : [ pure $ RenderTextCell $ totalCells s ]
    }
  where
    newState = addTextCell s

update (RenderCodeCell i) s =
    s `onlyEffects`
    [ liftEff
      $ makeCodeEditor s.editorChanges i
    ]

update (RenderTextCell i) s =
    s `onlyEffects`
    [ liftEff
      $ makeTextEditor s.editorChanges i
    ]

update RenderAllCells s =
  {state : modifiedState , effects: map render s.cells}
  where
    render cell = case cell of
      Cell {cellType : TextCell, cellId : i, cellContent : _ } ->
        liftEff $ loadTextEditor s.editorChanges i
      Cell {cellType : CodeCell, cellId : i, cellContent : _} ->
        liftEff $ makeCodeEditor s.editorChanges i
    modifiedState = 
        { currentCell : 0
        , cells : s.cells
        , editorChanges : s.editorChanges
        }

update (SaveContent cId newContent) s =
    noEffects
    $ saveContent cId (spy newContent) s

update (RemoveCell cId) s =
    noEffects
    $ removeCell cId s

update (SetCurrentCell cId) s =
    noEffects
    $ s { currentCell = cId }

update NoOp s =
    noEffects
    $ s
