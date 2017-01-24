module Global.Effects where

import WebSocket (WEBSOCKET)
import DOM (DOM)

type GlobalEffects = (ws :: WEBSOCKET, dom :: DOM)