module Notebook.Packer where

import Prelude

import Notebook.Types
import Cells.Types as Cells
import Console.Types as Console
import Data.Array

initialNotebook :: Notebook
initialNotebook = Notebook
  { title: ""
  , subtitle: ""
  , author: ""
  , date: ""
  , cells: [] :: Array Cells.Cell
  , console: ">"
  , filepath: "Main.hs"
  , loaded : true
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
    , filepath : "Main.hs"
    , loaded : true
    }

unpackCells :: Notebook -> Cells.State -> Cells.State
unpackCells (Notebook n) state =
  state { cells = n.cells
        , currentCell = newCurrentCell
        }
  where
    newCurrentCell = min state.currentCell (length state.cells)

unpackConsole :: Notebook -> Console.State -> Console.State
unpackConsole (Notebook n) state =
  state { display = n.console }
