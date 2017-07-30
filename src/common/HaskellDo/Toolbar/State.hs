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

import System.Directory (listDirectory, doesFileExist, doesDirectoryExist, getHomeDirectory, createDirectory)
import System.FilePath ((</>))

import Control.Monad (filterM, unless)

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
    , newDirectoryPath = ""
    }


lastProjectFile :: FilePath
lastProjectFile = "lastproject"

update :: Action -> State -> Cloud State
update OpenProject state = do
    localIO $ openModal "#openProjectModal"
    update (NewPath (projectPath state)) state

update NewDirectoryModal state = do
  localIO $ openModal "#newDirectoryModal"
  return state

update LoadPackageYaml state = do
    localIO $ if projectOpened state
        then openModal "#packageEditorModal"
        else shakeErrorDisplay
    return state

update ClosePackageModal state = do
    localIO $ closeModal "#packageEditorModal"
    return state

update (NewDirectory path) state = return $ state { newDirectoryPath = projectPath state </> path }

update CreateNewDirectory state = do
  let path = newDirectoryPath state
  exists <- atRemote . localIO $ doesDirectoryExist path

  unless exists $
    atRemote . localIO $ createDirectory path

  update (NewPath (projectPath state)) state

update (NewPath newPath) state = do
    path <- pathOrLastOrHome newPath
    localIO $ setValueForId "#pathInput event input" path

    exists <- atRemote . localIO $ doesDirectoryExist path
    let newState = state { directoryExists = exists, projectPath = path }

    if exists
    then do
      (directories, files) <- directoriesAndFiles path

      let newState' = newState { directoryList = (directories, files) }

      isProject <- atRemote $ localIO $ doesFileExist (path </> "package.yaml")
      updateProjectAvailability newState' path isProject
    else do
      let newState' = newState { directoryList = ([], []) }
      return newState'
  where
    pathOrLastOrHome path = if null path
                      then do
                        exists <- atRemote . localIO $ doesFileExist lastProjectFile
                        home <- atRemote . localIO $ getHomeDirectory
                        -- if lastProjectFile exists and is not empty, use it
                        -- otherwise use the home directory
                        if exists
                        then do
                          content <- atRemote . localIO $ readFile lastProjectFile
                          return $ if null content then home else content
                        else
                          return home
                      else
                        return path

    directoriesAndFiles path = do
      list <- atRemote . localIO $ listDirectory path
      let visible = filter ((/= '.') . head) list
      directories <- atRemote . localIO $ filterM (doesDirectoryExist . (path </>)) visible
      files <- atRemote . localIO $ filterM (doesFileExist . (path </>)) visible

      return (directories, files)

    updateProjectAvailability currentState _ True = do
      localIO $ setHtmlForId "#creationDisplay" ""
      localIO $ setHtmlForId "#closeModalButton event .material-icons" "input"
      return $ currentState { createProject = False }

    updateProjectAvailability currentState path False = do
      localIO $ setHtmlForId "#creationDisplay" ("<p class=\"red-text\">No project found at " ++ path ++ ", it will be created.</p>")
      localIO $ setHtmlForId "#closeModalButton event .material-icons" "playlist_add"
      return $ currentState { createProject = True }

update (NewPackage newConfig) state = return state { projectConfig = newConfig }

update ToggleEditor state = do
    localIO toggleEditor
    return state

update ToggleError state = do
    localIO toggleError
    return state

update _ state = return state

shakeErrorDisplay :: IO ()
shakeErrorDisplay = shake "#errorDisplay"
