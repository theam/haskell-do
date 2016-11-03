module View where

import Prelude (const, show)
import Pux.Html
import Pux.Html.Events (onClick)
import Pux.Html.Attributes
import Types (State, Action(..))

navbar :: State -> Html Action
navbar state =
  nav
    [ className "navbar navbar-default" ]
    [ div
        [ className "container-fluid" ]
        [ div
            [ className "navbar-header" ]
            []
        , div
            [ className "" ]
            [ ul
                [ className "nav navbar-nav text-center" ]
                [ li [] [ a [ className "glyphicon glyphicon-file" ] [ text "" ] ]
                , li [] [ a [ className "glyphicon glyphicon-folder-open" ] [ text "" ] ]
                ]
            ]
        , ul
                [ className "nav navbar-nav navbar-right" ]
                [ li [] [ a [ className "glyphicon glyphicon-eye-open" ] [ text "" ] ]
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
