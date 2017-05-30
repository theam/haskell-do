module HaskellDo.Toolbar.State where

import Transient.Move
import HaskellDo.Toolbar.Types
import Foreign.Materialize
import Foreign.JQuery

initialState :: State
initialState = State
    { projectPath = ""
    , lastProject = ""
    , projectConfig = ""
    , projectOpened = False
    }

update :: Action -> State -> Cloud State
update OpenProject state = do
    localIO $ openModal "#openProjectModal"
    return state

update LoadPackageYaml state = do
    localIO $ if projectOpened state
        then openModal "#packageEditorModal"
        else shakeErrorDisplay
    return state

update ClosePackageModal state = do
    localIO $ closeModal "#packageEditorModal"
    return state

update (NewPath "") state = return state
update (NewPath newPath) state =
    if last newPath /= '/'
        then return state { projectPath = newPath ++ "/" }
        else return state { projectPath = newPath }

update (NewPackage newConfig) state = return state { projectConfig = newConfig }

update _ state = return state

shakeErrorDisplay :: IO ()
shakeErrorDisplay = shake "#errorDisplay"
