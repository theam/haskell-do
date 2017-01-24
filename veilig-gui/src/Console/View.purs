module Console.View where

import Prelude hiding (div)

import Pux.Html
import Pux.Html.Attributes
import Pux.Html.Events
import Data.Lens as Lens
import Console.Types

view :: State -> Html Action
view s = 
    pre 
        [ className "console", id_ "consoleWindow"]
        [ code
            []
            [ text $ s.buffer ]
        , input 
            [id_ "consoleInput", onKeyPress sendConsole, onChange updateConsole ]
            []
        ]
  where
    sendConsole   ev = if ev.charCode == 13 then (Send s.buffer) else NoOp
    updateConsole ev = Add ev.target.value