module Cells.View where

import Prelude (const, map, show, ($), (<<<), (<>))

import Cells.Types
import Pux.Html (Html, a, code, li, pre, text, textarea, ul)
import Pux.Html.Attributes (className, defaultValue, href, id_)
import Pux.Html.Events (onClick, onInput)
import Data.Lens as Lens


renderCell :: Cell -> Html Action
renderCell (Cell c) =
    case c.cellType of
        TextCell -> renderTextCell (Cell c)
        CodeCell -> renderCodeCell (Cell c)

renderTextCell :: Cell -> Html Action
renderTextCell (Cell c) =
    li
        [ id_     $ "outer-" <> show cId
        , className "text-cell"
        ]
        [ textarea
            [ id_          $ show cId
            , defaultValue $ c.cellContent
            ] []
        ]
  where
    cId = c.cellId

renderCodeCell :: Cell -> Html Action
renderCodeCell (Cell c) = wrapper $ 
    textarea
        [ id_          $ show cId
        , defaultValue $ c.cellContent
        ] []
  where
    cId = c.cellId
    wrapper content = li [] [ pre [] [ code [] [ content ] ] ]

renderCells :: State -> Array (Html Action)
renderCells s = map renderCell s.cells

cellsDisplay :: State -> Html Action
cellsDisplay appState = ul [] (renderCells appState)

addTextCellButton :: State -> Html Action
addTextCellButton _ = a
    [ onClick $ const AddTextCell
    , href "#"
    ]
    [ text "Text cell" ]

addCodeCellButton :: State -> Html Action
addCodeCellButton _ = a
    [ onClick $ const AddCodeCell
    , href "#"
    ]
    [ text "Code cell" ]