module Cells.View where

import Prelude (const, map, show, ($), (<<<), (<>))

import Cells.Types (Action(..), Cell, CellType(..), State, cellContent, cellId, cellType, cells)
import Pux.Html (Html, a, code, li, pre, text, textarea, ul)
import Pux.Html.Attributes (className, defaultValue, href, id_)
import Pux.Html.Events (onClick, onInput)
import Data.Lens as Lens


renderCell :: Cell -> Html Action
renderCell c =
    case Lens.view cellType c of
        TextCell -> renderTextCell c
        CodeCell -> renderCodeCell c

renderTextCell :: Cell -> Html Action
renderTextCell c =
    li
        [ id_     $ "outer-" <> show cId
        , className "text-cell"
        ]
        [ textarea
            [ id_          $ show cId
            , onInput      $ (\ev -> SaveContent cId ev.target.value)
            , defaultValue $ Lens.view cellContent c
            ] []
        ]
  where
    cId = Lens.view cellId c

renderCodeCell :: Cell -> Html Action
renderCodeCell c = wrapper $ 
    textarea
        [ onInput      $ (\ev -> SaveContent cId ev.target.value)
        , id_          $ show cId
        , defaultValue $ Lens.view cellContent c
        ] []
  where
    cId = Lens.view cellId c
    wrapper content = li [] [ pre [] [ code [] [ content ] ] ]

renderCells :: State -> Array (Html Action)
renderCells = map renderCell <<< Lens.view cells

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