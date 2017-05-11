module HaskellDo.Toolbar.State where

import Transient.Move
import HaskellDo.Toolbar.Types
import Foreign.Materialize

initialState :: State
initialState = State
    { projectPath = ""
    , lastProject = ""
    , projectConfig = ""
    }

update :: Action -> State -> Cloud State
update OpenProject state = do
    localIO $ openModal "#openProjectModal"
    return state

update LoadPackageYaml state = do
    localIO $ openModal "#packageEditorModal"
    return state

update (NewPath newPath) state =
    if last newPath /= '/' && newPath /= ""
        then return state { projectPath = newPath ++ "/" }
        else return state { projectPath = newPath }

update (NewPackage newConfig) state = return state { projectConfig = newConfig }

update _ state = return state
