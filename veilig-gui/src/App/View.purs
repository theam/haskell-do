module App.View where

import Prelude                  hiding (div)

import App.Types                as App
import Cells.View               as Cells
import Columns.View             as Columns
import Console.View             as Console

import Bootstrap.DropdownMenu   as DropdownMenu
import Bootstrap.TopFixedNavbar as TopFixedNavbar
import Bootstrap.Glyphicon      (Glyphicon(..))

import Data.Lens                as Lens
import Pux.Html                 (Html, div, forwardTo)

view :: App.State -> Html App.Action
view s =
    div
        []
        [ TopFixedNavbar.view 
            [ DropdownMenu.view GlyphiconPlus 
                [ forwardTo App.CellsAction $ Cells.addTextCellButton cellsSt
                , forwardTo App.CellsAction $ Cells.addCodeCellButton cellsSt
                ]
            , forwardTo App.ColumnsAction $ Columns.toggleColumnsButton columnsSt
            ]
        , forwardTo App.CellsAction $ Cells.cellsDisplay cellsSt
        , forwardTo App.ConsoleAction $ Console.view consoleSt
        ]
  where
    cellsSt = Lens.view App.cellsState s
    consoleSt = Lens.view App.consoleState s
    columnsSt = Lens.view App.columnsState s
