module Console.Types where

import Prelude

import Signal.Channel
import Data.Lens

data Action
    = Add String
    | Send String
    | NoOp

type State = 
    { buffer :: String
    , consoleChannel :: Channel Action
    }
