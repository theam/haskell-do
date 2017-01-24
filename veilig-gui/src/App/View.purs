module App.View where

import Prelude                  hiding (div)

import App.Types                as App
import Cells.View               as Cells
import Columns.View             as Columns
import Console.View             as Console

import Bootstrap.DropdownMenu   as DropdownMenu
import Bootstrap.TopFixedNavbar as TopFixedNavbar
import Bootstrap.Glyphicon      (Glyphicon(..))

import Pux.Html                 (Html, div, forwardTo)

view :: App.State -> Html App.Action
view s =
    div
        []
        [ TopFixedNavbar.view 
            [ DropdownMenu.view GlyphiconPlus 
                [ forwardTo App.CellsAction $ Cells.addTextCellButton s.cellsState
                , forwardTo App.CellsAction $ Cells.addCodeCellButton s.cellsState
                ]
            , forwardTo App.ColumnsAction $ Columns.toggleColumnsButton s.columnsState
            ]
        , forwardTo App.CellsAction $ Cells.cellsDisplay s.cellsState
        , forwardTo App.ConsoleAction $ Console.view s.consoleState
        ]