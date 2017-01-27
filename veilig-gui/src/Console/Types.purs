module Console.Types where

import Prelude

import Signal.Channel
import Data.Lens

data Action
    = Add String
    | Send String
    | PackAndSendToBackend String
    | NoOp

type State = 
    { buffer :: String
    , display :: String
    , consoleChannel :: Channel Action
    }
