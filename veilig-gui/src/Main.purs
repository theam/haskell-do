module Main where

import Prelude (Unit, bind)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Signal.Channel (CHANNEL)
import Pux (renderToDOM, fromSimple, start)
import State (update, initialAppState)
import View (view)
import Types (AppState)

main :: forall e. Eff (err :: EXCEPTION, channel :: CHANNEL | e) Unit
main = do
  app <- start
    { initialState: initialAppState
    , update: fromSimple update
    , view: view
    , inputs: []
    }
  renderToDOM "#app" app.html
