module Console.View where

import Prelude hiding (div)
import Pux.Html
import Pux.Html.Attributes
import Pux.Html.Events
import Types
import Data.Lens as L

view :: AppState -> Html Action
view appState = 
    let code = L.view (_notebook <<< _console) appState in
    div []
        [ textarea 
            [rows 8, cols 80, defaultValue code , id_ "console"]
            []
        ]
