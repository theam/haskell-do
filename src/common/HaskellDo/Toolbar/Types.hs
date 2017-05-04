module HaskellDo.Toolbar.Types where

data State = State
    { projectPath :: String
    } deriving (Read, Show)

data Action
    = Compile
    | OpenProject
    | NewPath String
    deriving (Read, Show)
