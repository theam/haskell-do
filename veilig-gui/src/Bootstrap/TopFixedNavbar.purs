module Bootstrap.TopFixedNavbar where

import Prelude hiding (div)

import Pux.Html (Html, div, nav, ul, li)
import Pux.Html.Attributes (className)

view :: ∀ action . Array (Html action) -> Html action
view contents =
  nav
    [ className "navbar navbar-default navbar-fixed-top" ]
    [ div
        [ className "container-fluid" ]
        [ div
            [ className "navbar-header" ]
            []
        , div
            []
            [ ul
                [ className "nav navbar-nav text-center" ]
                $ map intoLi contents
            ]
        ]
    ]
  where
    intoLi x = li [] [x]
