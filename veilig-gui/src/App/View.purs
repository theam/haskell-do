module App.View where

import App.Types as App
import Bootstrap.DropdownMenu as DropdownMenu
import Bootstrap.TopFixedNavbar as TopFixedNavbar
import Cells.View as Cells
import Columns.View as Columns
import Console.View as Console
import Bootstrap.Glyphicon (Glyphicon(..))
import Pux.Html (Html, div, forwardTo)
import Pux.Html.Attributes (className)
import Prelude hiding (div)

view :: App.State -> Html App.Action
view s =
    div
        [ className "container" ]
        [ TopFixedNavbar.view 
            [ DropdownMenu.view GlyphiconPlus 
                [ map App.CellsAction $ Cells.addTextCellButton s.cellsState
                , map App.CellsAction $ Cells.addCodeCellButton s.cellsState
                ]
            , map App.ColumnsAction $ Columns.toggleColumnsButton s.columnsState
            ]
        , Columns.view 
            (map App.CellsAction $ Cells.cellsDisplay s.cellsState)
            (map App.ConsoleAction $ Console.view s.consoleState)
        ]

