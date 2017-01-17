module Navbar.View where

import Prelude (const, ($), (<>))

import UIUtils
import Types (AppState, Action(..))
import Pux.Html (Html, ul, text, a, li, div, nav)
import Pux.Html.Events (onClick)
import Pux.Html.Attributes (className, href, role, aria, attr)

additionMenu :: Html Action
additionMenu = 
    dropdownMenu
        GlyphiconPlus
        [ a
            [ onClick (const AddCodeCell)
            , href "#"
            ]
            [ text "Code cell" ]
        , a
            [ onClick (const AddTextCell)
            , href "#"
            ]
            [ text "Text cell" ]
        ]

view :: AppState -> Html Action
view appState = topFixedNavbar additionMenu
