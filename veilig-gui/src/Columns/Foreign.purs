module Columns.Foreign where

import Prelude

import Control.Monad.Eff
import DOM (DOM)

foreign import toggleColumns :: ∀ e . Eff ( dom :: DOM | e ) Unit
