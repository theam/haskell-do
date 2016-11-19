module Cells.View where

import Prelude (map, show, (<<<))

import Types (AppState, Action(..), Cell(..), CellType(..), _cells, _notebook)
import Pux.Html (Html, ul, text, li, textarea)
import Pux.Html.Events (onInput)
import Pux.Html.Attributes (defaultValue, id_)
import Data.Lens as L


renderCell :: Cell -> Html Action
renderCell (Cell c@{ cellType : TextCell}) = renderTextCell (Cell c)
renderCell (Cell c@{ cellType : CodeCell}) = renderCodeCell (Cell c)
renderCell (Cell c@{ cellType : DisplayCell}) = renderDisplayCell (Cell c)

renderTextCell :: Cell -> Html Action
renderTextCell (Cell c) =
    li
        []
        [ textarea
            [ id_ (show c.cellId)
            , onInput (CheckInput c.cellId)
            , defaultValue c.cellContent
            ]
            []
        ]

renderCodeCell :: Cell -> Html Action
renderCodeCell (Cell c) =
    li
        []
        [ textarea
            [ onInput (CheckInput c.cellId)
            , id_ (show c.cellId)
            , defaultValue c.cellContent
            ]
            []
        ]

renderDisplayCell :: Cell -> Html Action
renderDisplayCell (Cell c) =
    li [] [ text "" ]

renderCells :: AppState -> Array (Html Action)
renderCells = map renderCell <<< L.view (_notebook <<< _cells)

view :: AppState -> Html Action
view appState = ul [] (renderCells appState)
