module HaskellDo.Toolbar.Types where

data State = State
    { projectPath   :: String
    , lastProject   :: String
    , projectConfig :: String
    , projectOpened :: Bool
    , createProject :: Bool
    } deriving (Read, Show)

data Action
    = Compile
    | OpenProject
    | LoadPackageYaml
    | NewPath String
    | NewPackage String
    | LoadProject
    | SavePackage
    | ClosePackageModal
    deriving (Read, Show)
