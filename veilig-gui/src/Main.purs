module Main where

import Prelude
import App.State as App
import Types
import WebSocket
import Signal
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Var (($=))
import Data.Either (Either(Left))
import Data.Maybe (Maybe(Nothing))
import Pux (CoreEffects, renderToDOM, fromSimple, start)
import Signal.Channel (subscribe, channel, CHANNEL)
import DOM
import View (view)

main :: Eff (CoreEffects (ws :: WEBSOCKET, dom :: DOM)) Unit
main = do
    wsInput <- channel NoOp
    appState <- App.initialAppState wsInput "ws://127.0.0.1:3000"
    let wsSignal = subscribe wsInput :: Signal Action
    app <- start
        { initialState: appState
        , update: update
        , view: view
        , inputs: [wsSignal]
        }
    renderToDOM "#app" app.html
