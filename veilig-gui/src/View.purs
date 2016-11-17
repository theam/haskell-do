module View where

import Pux.Html.Attributes
import Types
import Prelude (const, show)
import Pux.Html.Events (onChange, onClick, onInput, onLoad)
import Pux.Html hiding (map)
import Prelude hiding (div)
import Data.Lens as L

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

renderTextCell :: Cell -> Html Action
renderTextCell (Cell c@{ cellType : TextCell }) =
    li
        []
        [ p
            [ contentEditable "true"
            , id_ (show c.cellId)
            , onInput (CheckInput c.cellId)
            ]
            [ text c.cellContent ]
        ]
renderTextCell (Cell c@{ cellType : CodeCell }) =
  li
    []
    [ pre
        []
        [ textarea
            [ onClick (const $ RenderCodeCell c.cellId)
            , onChange (CheckInput c.cellId)
            , id_ (show c.cellId)
            , defaultValue "Insert code"
            ]
            []
        ]
    ]
renderTextCell (Cell c@{ cellType : DisplayCell }) =
    li [] [ text "" ]

renderCells :: AppState -> Array (Html Action)
renderCells = map renderTextCell <<< L.view (_notebook <<< _cells)--appState.notebook.cells

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
