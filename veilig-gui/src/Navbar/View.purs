module Navbar.View where

import Prelude (const, ($), (<>))

import Types (AppState, Action(..))
import Pux.Html (Html, ul, text, a, li, div, nav)
import Pux.Html.Events (onClick)
import Pux.Html.Attributes (className, href, role, aria, attr)

editButton :: AppState -> Html Action
editButton appState =
  let
    icon = if true then "pencil" else "eye-open"
    cname = "glyphicon glyphicon-" <> icon
  in
    li [] [ a [ className cname, onClick (const ToggleEdit) ] [ text "" ] ]

additionMenu :: Html Action
additionMenu =
  li
    [ className "dropdown" ]
    [ a
        [ className "dropdown-toggle glyphicon glyphicon-plus"
        , attr "data-toggle" "dropdown"
        ]
        [ text "" ]
    , ul
        [ className "dropdown-menu"
        , aria "labelledby" "dLabel"
        , role "menu"
        ]
        [ li
            []
            [ a
                [ onClick (const AddTextCell)
                , href "#"
                ]
                [ text "Text cell" ]
            ]
        , li
            []
            [ a
                [ onClick (const AddCodeCell)
                , href "#"
                ]
                [ text "Code cell" ]
            ]
        ]
    ]

view :: AppState -> Html Action
view appState =
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
                [ li [] [ a [ className "glyphicon glyphicon-play", onClick (const $ CheckNotebook) ] [ text "" ] ]
                , additionMenu
                ]
            ]
        , ul
                [ className "nav navbar-nav navbar-right" ]
                [ editButton appState
                ]
        ]
    ]
