module HaskellDo.Toolbar.State where

import System.Directory (doesFileExist)

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
    , createProject = False
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
update (NewPath newPath) state = do
    newState <- if last newPath /= '/'
                then return state { projectPath = newPath ++ "/" }
                else return state { projectPath = newPath }
    isProject <- atRemote $ localIO $ doesFileExist (projectPath newState ++ "package.yaml")
    if isProject
        then do
            localIO $ setHtmlForId "#creationDisplay" ""
            localIO $ setHtmlForId "#closeModalButton event .material-icons" "input"
            return $ newState { createProject = False }
        else do
            localIO $ setHtmlForId "#creationDisplay" ("No project found at " ++ projectPath newState ++ ", it will be created.")
            localIO $ setHtmlForId "#closeModalButton event .material-icons" "playlist_add"
            return $ newState { createProject = True }

update (NewPackage newConfig) state = return state { projectConfig = newConfig }

update _ state = return state

shakeErrorDisplay :: IO ()
shakeErrorDisplay = shake "#errorDisplay"
