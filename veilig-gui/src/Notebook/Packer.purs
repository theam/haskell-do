module Notebook.Packer where

import Notebook.Types
import Cells.Types as Cells
import Console.Types as Console
import Data.Lens

initialNotebook :: Notebook
initialNotebook = Notebook
  { title: ""
  , subtitle: ""
  , author: ""
  , date: ""
  , cells: [] :: Array Cells.Cell
  , console: ">"
  }

-- | Grabs the cells and the console from their
--   respective states, and packs everything into
--   a Notebook.
pack :: Cells.State -> Console.State -> Notebook
pack cellsState consoleState = Notebook
    { title : ""
    , subtitle : ""
    , date : ""
    , author : ""
    , cells : cellsState.cells
    , console : consoleState.buffer
    }
