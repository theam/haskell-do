module Columns.State where

import Prelude
import Pux
import DOM
import Columns.Types
import Columns.Foreign
import Global.Effects
import Control.Monad.Eff.Class (liftEff)

update :: Update State Action GlobalEffects
update Toggle = flip onlyEffects [ do
        liftEff toggleColumns
        pure NoOp
    ]

update NoOp   = noEffects

