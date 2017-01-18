module Components.Navbar where

import Prelude (const, ($), (<>))

import UIUtils
import Types (Action(..))
import Pux.Html (Html, ul, text, a, li, div, nav)
import Pux.Html.Events (onClick)
import Pux.Html.Attributes (className, href, role, aria, attr)

data Action
    = AddCodeCell
    | AddTextCell

view :: ∀ appState . appState -> Html Action
view _ = topFixedNavbar additionMenu
  where
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

