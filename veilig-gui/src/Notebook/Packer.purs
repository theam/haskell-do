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

unpackCells :: Notebook -> Cells.State -> Cells.State
unpackCells (Notebook n) state = 
  state { cells = n.cells
        , currentCell = newCurrentCell
        }
  where
    biggerThan (Cells.CellId i) i' = i > i'
    newCurrentCell =
        if state.currentCell `biggerThan` length state.cells
            then Cells.CellId $ length state.cells
            else state.currentCell

unpackConsole :: Notebook -> Console.State -> Console.State
unpackConsole (Notebook n) state =
  state { buffer = n.console }