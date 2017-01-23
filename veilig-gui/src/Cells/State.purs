module Cells.State where

import Prelude
import Control.Monad.Eff.Class (liftEff)

import Pux
import Cells.Types
import Cells.Foreign
import Data.Lens
import DOM
import Signal.Channel

initialState :: Channel Action -> State
initialState chan = State
    { _currentCell   : CellId 0
    , _cells         : [] :: Array Cell
    , _editorChanges : chan
    }

addCodeCell :: State -> State
addCodeCell s = insertAfter (view currentCell s) (newCodeCell $ CellId $ totalCells s) s

addTextCell :: State -> State
addTextCell s = insertAfter (view currentCell s) (newTextCell $ CellId $ totalCells s) s

saveContent :: CellId -> String -> State -> State
saveContent cId newContent = over cells (\cell' -> map updateCell cell')
  where
    isCorrectCell c = view cellId c == cId
    updateCell c =
        if isCorrectCell c
            then (cellContent .~ newContent) c
            else c

update :: Update State Action ( dom :: DOM )
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
      $ makeCodeEditor (view editorChanges s) i
    ]

update (RenderTextCell i) s  =
    s `onlyEffects`
    [ liftEff
      $ makeTextEditor (view editorChanges s) i
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

