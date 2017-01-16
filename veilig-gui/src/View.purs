module View where

import Prelude

import Pux.Html.Attributes (className)
import Types
import Pux.Html (Html, div, nav, text)
import Navbar.View as Navbar
import Cells.View as Cells
import Data.Lens as L
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

columns :: Html Action -> Html Action -> Html Action
columns leftSide rightSide =
    div
        [ className "container-fluid" ]
        [ div
            [ className "row" ]
            [ div
                [ className "col-lg-4 col-lg-push-8"]
                [ nav
                    [ className "navbar navbar-default navbar-fixed-side" ]
                    [ rightSide ]
                ]
            , div
                [ className "col-lg-8 col-lg-pull-4" ]
                [ leftSide ]
            ]
        ]

view :: AppState -> Html Action
view appState =
    div
        []
        [ Navbar.view appState
        , columns 
            (Cells.view appState)
            (Console.view appState)
        ]
