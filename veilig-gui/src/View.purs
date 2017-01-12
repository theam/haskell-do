module View where

import Prelude (($))

import Pux.Html.Attributes (className)
import Types
import Pux.Html (Html, div)
import Navbar.View as Navbar
import Cells.View as Cells
import Console.View as Console

boilerplate :: forall a. Html a -> Html a
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

view :: AppState -> Html Action
view appState =
    div
        []
        [ Navbar.view appState
        , boilerplate $ Cells.view appState
        , boilerplate $ Console.view appState
        ]
