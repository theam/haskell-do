module Console.View where

import Prelude hiding (div)
import Pux.Html
import Pux.Html.Attributes
import Pux.Html.Events
import Types
import Data.Lens as L

view :: AppState -> Html Action
view appState = 
    let 
        cde = L.view (_notebook <<< _console) appState
        inp = L.view _consoleBuffer appState
    in
    pre [ className "console", id_ "consoleWindow"]
        [ code
            [ ]
            [ text $ cde ]
        , input 
            [id_ "consoleInput", onKeyPress sendConsole, onChange updateConsole ]
            []
        ]

sendConsole ev = 
    if ev.charCode == 13
        then CheckNotebook
        else NoOp

updateConsole ev =
    AddToConsole ev.target.value