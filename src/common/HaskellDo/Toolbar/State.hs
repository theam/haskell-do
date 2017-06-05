{-
 - Copyright (c) 2017 The Agile Monkeys S.L. <hackers@theam.io>
 -
 - Licensed under the Apache License, Version 2.0 (the "License");
 - you may not use this file except in compliance with the License.
 - You may obtain a copy of the License at
 -
 -     http://www.apache.org/licenses/LICENSE-2.0
 -
 - Unless required by applicable law or agreed to in writing, software
 - distributed under the License is distributed on an "AS IS" BASIS,
 - WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 - See the License for the specific language governing permissions and
 - limitations under the License.
 -}
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
            localIO $ setHtmlForId "#creationDisplay" ("<p class=\"red-text\">No project found at " ++ projectPath newState ++ ", it will be created.</p>")
            localIO $ setHtmlForId "#closeModalButton event .material-icons" "playlist_add"
            return $ newState { createProject = True }

update (NewPackage newConfig) state = return state { projectConfig = newConfig }

update _ state = return state

shakeErrorDisplay :: IO ()
shakeErrorDisplay = shake "#errorDisplay"
