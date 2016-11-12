module Main where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Prelude (Unit, bind, const)
import Pux (CoreEffects, renderToDOM, fromSimple, start)
import Signal.Channel (CHANNEL)
import State
import Types
import View (view)

main :: forall e. Eff (CoreEffects (makeEditor :: MAKEEDITOR)) Unit
main = do
  app <- start
    { initialState: initialAppState
    , update: update
    , view: view
    , inputs: []
    }
  renderToDOM "#app" app.html
