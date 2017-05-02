module HaskellDo.SimpleMDE.Types where

data Action
    = NewContent String
    deriving (Read, Show)

data State = State
    { content :: String
    } deriving (Read, Show)
