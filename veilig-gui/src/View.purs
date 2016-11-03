module View where

import Prelude (const, show)
import Pux.Html
import Pux.Html.Events (onClick)
import Pux.Html.Attributes
import Types (State, Action(..))

navbar :: State -> Html Action
navbar state =
  div
    [ className "navbar navbar-default" ]
    [ div
        [ className "container-fluid" ]
        [ div
            [ className "navbar-header" ]
            [ a [ className "navbar-brand", href "#" ] [ text "Veilig" ]
            ]
        ]
    ]

view :: State -> Html Action
view state =
  div
    []
    [ navbar state
    , button [ onClick (const Increment) ] [ text "Increment" ]
    , span [] [ text (show state) ]
    , button [ onClick (const Decrement) ] [ text "Decrement" ]
    ]
