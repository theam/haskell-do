module Console.State where

import Prelude

import Pux
import Signal.Channel
import Console.Types
import Data.Lens ((.~))
import Global.Effects

update :: Update State Action GlobalEffects
update (Add content) s = noEffects $ s { buffer = content }
update (Send _) s = noEffects $ s
update NoOp s = noEffects $ s
