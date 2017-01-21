module View where

import Prelude

import Pux.Html.Attributes (className)
import Types
import Pux.Html (Html, div, nav, text)
import Components.Navbar as Navbar
import Cells.View as Cells
import Columns.View as Columns
import Console.View as Console

{--------  IS THIS EVEN NEEDED?

-- TODO: Remove this

boilerplate :: ∀ action . Html action -> Html action
boilerplate content =
    div
        [ className "container" ]
        [ div
            [ className "row" ]
            [ div
                [ className "col-md-12" ]
                [ content ]
            ]
        ]
------------------------------}

view :: AppState -> Html Action
view appState =
    div
        []
        [ forwardTo NavbarAction $ Navbar.view appState
        , forwardTo ColumnsAction $ 
            Columns.view (Lens.view columnsState appState) 
                (Cells.cellsDisplay $ Lens.view cellsState appState)
                (Console.view appState)
        ]
