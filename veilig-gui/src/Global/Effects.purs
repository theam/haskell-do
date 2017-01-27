module Global.Effects where

import DOM (DOM)
import Signal.Channel (CHANNEL)
import WebSocket (WEBSOCKET)

type GlobalEffects = (ws :: WEBSOCKET, dom :: DOM)

