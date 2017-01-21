module Console.State where

import Prelude

import Pux
import Signal.Channel
import Console.Types
import Data.Lens ((.~))

update :: Update State Action ( channel :: CHANNEL )
update (Add content) s = noEffects $ (buffer .~ content) s
update (Send _) s = noEffects $ s
update NoOp s = noEffects $ s
