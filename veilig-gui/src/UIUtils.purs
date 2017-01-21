module UIUtils where

import Prelude hiding (div)

import Pux.Html (Html, ul, text, a, li, div, nav)
import Pux.Html.Attributes (className, role, aria, attr)

data Glyphicon
    = GlyphiconPlus

instance showGlyphicon :: Show Glyphicon where
    show GlyphiconPlus = "glyphicon glyphicon-plus"

dropdownMenu :: ∀ action .  Glyphicon -> Array (Html action) -> Html action
dropdownMenu icon contents =
  li
    [ className "dropdown" ]
    [ a
        [ className $ "dropdown-toggle" <> show icon
        , attr "data-toggle" "dropdown"
        ]
        [ text "" ]
    , ul
        [ className "dropdown-menu"
        , aria "labelledby" "dLabel"
        , role "menu"
        ]
        (map intoListItem contents)
    ]
  where
    intoListItem item = li [] [ item ]


topFixedNavbar :: ∀ action . Html action -> Html action
topFixedNavbar contents =
  nav
    [ className "navbar navbar-default navbar-fixed-top" ]
    [ div
        [ className "container-fluid" ]
        [ div
            [ className "navbar-header" ]
            []
        , div
            [ className "" ]
            [ ul
                [ className "nav navbar-nav text-center" ]
                [ contents
                ]
            ]
        ]
    ]
