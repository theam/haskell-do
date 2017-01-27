module Columns.View where

import Prelude (const, ($), (<>))

import Pux.Html (Html, a, div, nav, text)
import Pux.Html.Attributes (className, href, id_, style)
import Pux.Html.Events (onClick)
import Data.Tuple (Tuple(..))
import Columns.Types (Action(..), State)

columnStyle :: Array (Tuple String String)
columnStyle =
    [ Tuple "transition" "all 0.05s" ]

view :: ∀ action . Html action -> Html action -> Html action
view leftSide rightSide =
    div
        [ className "container-fluid" ]
        [ div
            [ className "row" ]
            [ div
                [ id_ "right-column"
                , style $ columnStyle
                , className "col-lg-4 col-lg-push-8"
                ]
                [ nav
                    [ className "navbar navbar-default navbar-fixed-side" ]
                    [ rightSide ]
                ]
            , div
                [ id_ "left-column"
                , style columnStyle 
                , className "col-lg-8 col-lg-pull-4"
                ]
                [ leftSide ]
            ]
        ]

toggleColumnsButton :: State -> Html Action
toggleColumnsButton s = a
    [ onClick $ const Toggle 
    , href "#"
    ]
    [ text "Toggle column" ]