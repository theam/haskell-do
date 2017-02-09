module Console.Foreign where

import Prelude

import Control.Monad.Eff
import DOM

foreign import animateSaveButton :: ∀ eff . Eff (dom :: DOM |eff) Unit