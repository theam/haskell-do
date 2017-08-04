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
{-# LANGUAGE LambdaCase #-}
module HaskellDo.State where

import Control.Exception (try, SomeException)
import Control.Monad (when)

import Transient.Move

import System.FilePath ((</>))

import HaskellDo.Types
import qualified HaskellDo.CodeMirror.State as CodeMirror
import qualified HaskellDo.CodeMirror.Types as CodeMirror
import qualified Foreign.CodeMirror as CodeMirror
import qualified HaskellDo.Compilation.State as Compilation
import qualified HaskellDo.Compilation.Types as Compilation
import qualified HaskellDo.Toolbar.State as Toolbar
import qualified HaskellDo.Toolbar.Types as Toolbar
import qualified Foreign.JQuery as JQuery

initialAppState :: AppState
initialAppState = AppState
  { codeMirrorState = CodeMirror.initialState
  , compilationState = Compilation.initialState
  , toolbarState = Toolbar.initialState
  }

update :: Action -> AppState -> Cloud AppState
update action appState = _preUpdate action appState >>= _update action >>= _postUpdate action

_preUpdate :: Action -> AppState -> Cloud AppState
_preUpdate _ appState = do
  let cs = compilationState appState
      newCS = cs { Compilation.dirtyCompile = False }
  return appState { compilationState = newCS }

_postUpdate :: Action -> AppState -> Cloud AppState
_postUpdate _ = return

_update :: Action -> AppState -> Cloud AppState
_update (CodeMirrorAction action) appState = do
    newCodeMirrorState <- CodeMirror.update action (codeMirrorState appState)
    let newContent = CodeMirror.content newCodeMirrorState
    _ <- atRemote $ Compilation.update
        (Compilation.WriteWorkingFile newContent)
        (compilationState appState)
    compileShortcutPressed <- localIO CodeMirror.cmdOrCtrlReturnPressed
    let newState = appState
            { codeMirrorState = newCodeMirrorState
            }
    if compileShortcutPressed
        then update (ToolbarAction Toolbar.Compile) newState
        else return newState

_update (ToolbarAction Toolbar.Compile) appState = do
    let tbState = toolbarState appState
    if Toolbar.projectOpened tbState
        then do
            localIO $ JQuery.show ".dimmedBackground"
            newCompilationState <- atRemote $ Compilation.update
                Compilation.Compile
                (compilationState appState)
            localIO $ JQuery.hide ".dimmedBackground"
            return appState
                { compilationState = newCompilationState
                }
        else do
            localIO Toolbar.shakeErrorDisplay
            return appState

_update (ToolbarAction Toolbar.LoadProject) appState = do
    localIO $ JQuery.hide "#dependencyMessage"
    let tbState = toolbarState appState
    let cmpState = compilationState appState
    let projectPath = Compilation.projectPath (compilationState appState)
    let filePath = Compilation.workingFile (compilationState appState)
    atRemote $ localIO $
        when (Toolbar.createProject tbState) (Compilation.makeNewProject projectPath)
    readAtRemote (projectPath </> filePath) >>= \case
        Left _ -> do
            let newTbState = tbState { Toolbar.projectOpened = False }
            let newCmpState = cmpState
                    { Compilation.compilationError = "Couldn't find Haskell.do project at "
                                                   ++ projectPath
                                                   ++ ". Was it created correctly?"
                    }
            localIO $ CodeMirror.setMDEContent ""
            localIO $ JQuery.setHtmlForId "#outputDisplay" ""
            return appState
                { toolbarState = newTbState
                , compilationState = newCmpState
                }
        Right contents -> do
            let editorState = codeMirrorState appState
            let parsedContents = unlines . drop 4 $ lines contents
            let newEditorState = editorState { CodeMirror.content = parsedContents }
            let newTbState = tbState { Toolbar.projectOpened = True }
            let newCmpState = cmpState { Compilation.compilationError = "" }
            localIO $ CodeMirror.setMDEContent parsedContents
            localIO $ JQuery.hide "#errorDisplay" -- Hide error while dependencies load
            localIO $ JQuery.setHtmlForId "#outputDisplay" ""
            let stateAfterOpening =  appState
                        { codeMirrorState = newEditorState
                        , toolbarState = newTbState
                        , compilationState = newCmpState
                        }
            localIO $ JQuery.show "#dependencyMessage"
            newState <- update (ToolbarAction Toolbar.Compile) stateAfterOpening
            localIO $ JQuery.hide "#dependencyMessage"
            localIO $ JQuery.show "#errorDisplay" -- Show it after they finished
            return newState

_update (ToolbarAction Toolbar.LoadPackageYaml) appState = do
    let projectPath = Compilation.projectPath (compilationState appState)
    contents <- atRemote $ localIO $ readFile (projectPath </> "package.yaml")
    let tbState = toolbarState appState
    let tbState' = tbState { Toolbar.projectConfig = contents }
    localIO $ JQuery.setValueForId "#packageTextArea event textArea" contents
    _ <- Toolbar.update Toolbar.LoadPackageYaml tbState
    return appState { toolbarState = tbState' }

_update (ToolbarAction Toolbar.SavePackage) appState = do
    let projectPath = Compilation.projectPath (compilationState appState)
    let tbState = toolbarState appState
    atRemote $ localIO $ writeFile (projectPath </> "package.yaml") (Toolbar.projectConfig tbState)
    _ <- Toolbar.update Toolbar.ClosePackageModal tbState
    localIO $ JQuery.show "#dependencyMessage"
    newState <- update (ToolbarAction Toolbar.Compile) appState
    localIO $ JQuery.hide "#dependencyMessage"
    return newState

_update (ToolbarAction action) appState = do
    newToolbarState <- Toolbar.update action (toolbarState appState)
    let cs = compilationState appState
    let newCompilationState = cs
            { Compilation.projectPath = Toolbar.projectPath newToolbarState
            }
    return appState { compilationState = newCompilationState, toolbarState = newToolbarState }

readAtRemote :: FilePath -> Cloud (Either String String)
readAtRemote path = atRemote . localIO $
    maybeRead path
    >>= \case
        Nothing -> return (Left $ "Could not open file " ++ path)
        Just txt -> return (Right txt)

maybeRead :: FilePath -> IO (Maybe String)
maybeRead path =
    try (readFile path)
    >>= handleRead

handleRead :: Either SomeException String -> IO (Maybe String)
handleRead = \case
    Left _ -> return Nothing
    Right txt -> return (Just txt)
