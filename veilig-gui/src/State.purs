module State where

import Prelude
import Types

update :: Action -> State -> State
update Increment count = count + 1
update Decrement count = count - 1
