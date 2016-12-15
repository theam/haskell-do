module Cells.View where

import Data.Lens as L
import Data.Maybe (Maybe(Just))
import Data.String (stripSuffix, Pattern(Pattern), stripPrefix)
import Prelude (map, show, (<<<), ($), (<>), (>>=))
import Pux.Html (img, data_, object, pre, Html, ul, text, li, textarea, code)
import Pux.Html.Attributes (src, className, defaultValue, id_)
import Pux.Html.Events (onInput)
import Types (AppState, Action(..), Cell(..), CellType(..), _cells, _notebook)


renderCell :: Cell -> Html Action
renderCell (Cell c@{ cellType : TextCell}) = renderTextCell (Cell c)
renderCell (Cell c@{ cellType : CodeCell}) = renderCodeCell (Cell c)
renderCell (Cell c@{ cellType : DisplayCell}) = renderDisplayCell (Cell c)

renderTextCell :: Cell -> Html Action
renderTextCell (Cell c) =
    li
        [ id_ $ "outer-" <> show c.cellId, className "text-cell"]
        [textarea
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
            [ case stripPrefix (Pattern "\"svg:") c.cellContent >>= stripSuffix (Pattern "\"") of
                Just s -> rSvg s
                _ -> rText c
            ]
        ]
  where
    rText c = text c.cellContent
    rSvg file = img [src file] []

renderCells :: AppState -> Array (Html Action)
renderCells = map renderCell <<< L.view (_notebook <<< _cells)

view :: AppState -> Html Action
view appState = ul [] (renderCells appState)
