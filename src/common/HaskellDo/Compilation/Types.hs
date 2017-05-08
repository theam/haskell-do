module HaskellDo.Compilation.Types where

data State = State
    { compiledOutput   :: String
    , compilationError :: String
    , projectPath      :: String
    , workingFile      :: String
    } deriving (Read, Show)

data Action
    = Compile
    | WriteWorkingFile String
    | GetLastProject
    deriving (Read, Show)
