module HaskellDo.Toolbar.State where

import Transient.Move
import HaskellDo.Toolbar.Types
import Foreign.Materialize

initialState :: State
initialState = State
    { projectPath = ""
    , lastProject = ""
    }

update :: Action -> State -> Cloud State
update OpenProject state = do
    localIO $ openModal "#openProjectModal"
    return state

update (NewPath newPath) state =
    if last newPath /= '/'
        then return state { projectPath = newPath ++ "/" }
        else return state { projectPath = newPath }

update _ state = return state
