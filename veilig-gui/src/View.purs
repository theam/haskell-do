module View where

import Pux.Html hiding (map)
import Pux.Html.Attributes
import Prelude (const, show)
import Pux.Html.Events (onClick)
import Types
import Prelude hiding (div)

editButton :: AppState -> Html Action
editButton appState =
  let
    icon = if appState.editing then "pencil" else "eye-open"
    cname = "glyphicon glyphicon-" <> icon
  in
    li [] [ a [ className cname, onClick (const ToggleEdit) ] [ text "" ] ]

navbar :: AppState -> Html Action
navbar appState =
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
                , li [] [ a [ className "glyphicon glyphicon-import" ] [ text "" ] ]
                ]
            ]
        , ul
                [ className "nav navbar-nav navbar-right" ]
                [ editButton appState
                ]
        ]
    ]

renderTextCell :: Cell -> Html Action
renderTextCell (TextCell s) = li [] [ p [ contentEditable "true" ] [ text s ] ]
renderTextCell _ = text ""

renderCells :: AppState -> Array (Html Action)
renderCells appState = map renderTextCell appState.notebook.cells

view :: AppState -> Html Action
view appState =
  div
    []
    [ navbar appState
    , div
        [ className "container", id_ "editor" ]
        [ div
            [ className "row" ]
            [ div
                [ className "col-md-12" ]
                [ ul
                    []
                    $ renderCells appState
                ]
            ]
        ]
    ]
