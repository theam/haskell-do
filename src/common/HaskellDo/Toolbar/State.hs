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

import System.Directory (listDirectory, doesFileExist, doesDirectoryExist, getHomeDirectory)
import System.FilePath ((</>))

import Control.Monad (filterM)

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
    , directoryExists = False
    , directoryList = ([], [])
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

update (NewPath newPath) state = do
    path <- if null newPath
            then
              atRemote . localIO $ getHomeDirectory
            else
              return newPath

    localIO $ setValueForId "#pathInput event input" path

    exists <- atRemote . localIO $ doesDirectoryExist path
    let newState = state { directoryExists = exists, projectPath = path }

    if exists
    then do
      list <- atRemote . localIO $ listDirectory path
      let visible = filter ((/= '.') . head) list
      directories <- atRemote . localIO $ filterM (doesDirectoryExist . (path </>)) visible
      files <- atRemote . localIO $ filterM (doesFileExist . (path </>)) visible

      let newState' = newState { directoryList = (directories, files) }

      isProject <- atRemote $ localIO $ doesFileExist (path </> "package.yaml")
      if isProject
          then do
              localIO $ setHtmlForId "#creationDisplay" ""
              localIO $ setHtmlForId "#closeModalButton event .material-icons" "input"
              return $ newState' { createProject = False }
          else do
              localIO $ setHtmlForId "#creationDisplay" ("<p class=\"red-text\">No project found at " ++ path ++ ", it will be created.</p>")
              localIO $ setHtmlForId "#closeModalButton event .material-icons" "playlist_add"
              return $ newState' { createProject = True }
    else do
      let newState' = newState { directoryList = ([], []) }
      return newState'



update (NewPackage newConfig) state = return state { projectConfig = newConfig }

update _ state = return state

shakeErrorDisplay :: IO ()
shakeErrorDisplay = shake "#errorDisplay"
