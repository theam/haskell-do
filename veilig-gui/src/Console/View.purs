module Console.View where

import Prelude hiding (div)
import Pux.Html
import Pux.Html.Attributes
import Pux.Html.Events
import Types
import Data.Lens as Lens

view :: AppState -> Html Action
view appState = 
    pre 
        [ className "console", id_ "consoleWindow"]
        [ code
            []
            [ text $ consoleCode appState ]
        , input 
            [id_ "consoleInput", onKeyPress sendConsole, onChange updateConsole ]
            []
        ]
  where
    consoleCode      = Lens.view (_notebook <<< _console) 
    sendConsole   ev = if ev.charCode == 13 then CheckNotebook else NoOp
    updateConsole ev = AddToConsole ev.target.value