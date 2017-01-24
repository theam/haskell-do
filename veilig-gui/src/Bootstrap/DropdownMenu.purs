module Bootstrap.DropdownMenu where

import Prelude (map, show, ($), (<>))

import Bootstrap.Glyphicon (Glyphicon)
import Pux.Html (Html, a, li, text, ul)
import Pux.Html.Attributes (aria, attr, className, role)

view :: ∀ action . Glyphicon -> Array (Html action) -> Html action
view icon contents =
  li
    [ className "dropdown" ]
    [ a
        [ className $ "dropdown-toggle glyphicon " <> show icon
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

