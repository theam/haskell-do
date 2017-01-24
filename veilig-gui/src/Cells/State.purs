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

initialState :: Channel Action -> State
initialState chan = 
    { currentCell   : CellId 0
    , cells         : [] :: Array Cell
    , editorChanges : chan
    }

addCodeCell :: State -> State
addCodeCell s = insertAfter s.currentCell (newCodeCell $ CellId $ totalCells s) s

addTextCell :: State -> State
addTextCell s = insertAfter s.currentCell (newTextCell $ CellId $ totalCells s) s

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
    { state : addCodeCell s
    , effects : [ pure $ RenderCodeCell ( CellId $ totalCells s ) ]
    }

update AddTextCell s = 
    { state : addTextCell s
    , effects : [ pure $ RenderTextCell ( CellId $ totalCells s ) ]
    }

update (RenderCodeCell i) s  =
    s `onlyEffects`
    [ liftEff
      $ makeCodeEditor s.editorChanges i
    ]

update (RenderTextCell i) s  =
    s `onlyEffects`
    [ liftEff
      $ makeTextEditor s.editorChanges i
    ]

update (SaveContent cId newContent) s = 
    noEffects
    $ saveContent cId newContent s

update (RemoveCell cId) s             = 
    noEffects 
    $ removeCell cId s

update NoOp s                         = 
    noEffects
    $ s

