module HaskellDo.Toolbar.Types where

data State = State
    { projectPath :: String
    , lastProject :: String
    } deriving (Read, Show)

data Action
    = Compile
    | OpenProject
    | NewPath String
    | LoadProject
    deriving (Read, Show)
