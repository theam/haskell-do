module View where

import Prelude
import Pux.Html (Html, text, button, span, div)
import Pux.Html.Events (onClick)

import Types

view :: State -> Html Action
view count =
  div
    []
    [ button [ onClick (const Increment) ] [ text "Increment" ]
    , span [] [ text (show count) ]
    , button [ onClick (const Decrement) ] [ text "Decrement" ]
    ]
