module Columns.State where

import Prelude
import Control.Monad.Eff.Class (liftEff)

import Pux
import DOM
import Columns.Types
import Columns.Foreign

update :: Update State Action ( dom :: DOM )
update Toggle = flip onlyEffects [ do
        liftEff toggleColumns
        pure NoOp
    ]

update NoOp   = noEffects

