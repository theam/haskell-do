module Console.State where

import Prelude
import Control.Monad.Eff.Class (liftEff)

import Pux
import Signal.Channel
import Console.Types
import Data.Lens ((.~))
import Global.Effects

initialState :: Channel Action -> State
initialState chan =
    { buffer : ">"
    , display : ">"
    , consoleChannel : chan
    }

update :: Update State Action GlobalEffects
update (Add content) s =
    noEffects $ s { buffer = content }

update (Send _) s = 
    onlyEffects s [ do
            liftEff $ send s.consoleChannel (PackAndSendToBackend s.buffer)
            pure NoOp
        ]

update _ s = noEffects s
