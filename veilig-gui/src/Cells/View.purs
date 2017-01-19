module Cells.View
  ( cellsDisplay
  , addTextCellButton
  , addCodeCellButton
  )
where

import Prelude

import Cells.Types
import Pux.Html
import Pux.Html.Attributes
import Pux.Html.Events
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
            [ id_          $ show cellId
            , onInput      $ (\ev -> SaveContent cId ev.target.value)
            , defaultValue $ Lens.view cellContent c
            ] []
        ]
  where
    cId = Lens.view cellId c

renderCodeCell :: Cell -> Html Action
renderCodeCell c = wrapper $ 
    textarea
        [ onInput      $ SaveContent cId
        , id_          $ show cId
        , defaultValue $ Lens.view cellContent c
        ] []
  where
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