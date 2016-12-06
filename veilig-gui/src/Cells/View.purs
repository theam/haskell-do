module Cells.View where

import Data.Lens as L
import Prelude (map, show, (<<<))
import Pux.Html (pre, Html, ul, text, li, textarea, code)
import Pux.Html.Attributes (defaultValue, id_)
import Pux.Html.Events (onInput)
import Types (AppState, Action(..), Cell(..), CellType(..), _cells, _notebook)


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
        [ pre
            []
            [ code
                []
                [ textarea
                    [ onInput (CheckInput c.cellId)
                    , id_ (show c.cellId)
                    , defaultValue c.cellContent
                    ]
                    []
                ]
            ]
        ]

renderDisplayCell :: Cell -> Html Action
renderDisplayCell (Cell c) =
    li
        []
        [ code
            [ id_ (show c.cellId) ]
            [ text c.cellContent]
        ]

renderCells :: AppState -> Array (Html Action)
renderCells = map renderCell <<< L.view (_notebook <<< _cells)

view :: AppState -> Html Action
view appState = ul [] (renderCells appState)
