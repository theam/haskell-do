module Main where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Prelude (Unit, bind, const)
import Pux (renderToDOM, fromSimple, start)
import Signal.Channel (CHANNEL)
import State (update, initialAppState)
import Types (Action(ToggleEdit), AppState)
import View (view)

main :: forall e. Eff (err :: EXCEPTION, channel :: CHANNEL | e) Unit
main = do
  app <- start
    { initialState: initialAppState
    , update: fromSimple update
    , view: view
    , inputs: []
    }
  renderToDOM "#app" app.html
