module Console.Types where

import Prelude

import Signal.Channel

data Action
    = Add String
    | Save
    | Send String
    | PackAndSendToBackend
    | NoOp

type State = 
    { buffer :: String
    , display :: String
    , consoleChannel :: Channel Action
    }
