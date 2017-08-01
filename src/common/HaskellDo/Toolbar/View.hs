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
module HaskellDo.Toolbar.View where

import Prelude hiding (div, id)

import GHCJS.HPlay.View hiding (addHeader, atr, id, wlink)
import AxiomUtils
import qualified Ulmus

import qualified HaskellDo.Toolbar.FileSystemTree as FileSystemTree

import Control.Monad.IO.Class

import HaskellDo.Toolbar.Types
import Foreign.JQuery

toolbar :: Widget ()
toolbar = rawHtml $ do
    div ! atr "class" "fixed-action-btn horizontal" $ do
        a ! atr "class" "btn-floating btn-large purple" $
            i ! atr "class" "material-icons" $
                ("menu" :: String)
        ul $ do
            li ! id "openProjectButton" $ noHtml
            li ! id "compileButton" $ noHtml
            li ! id "packageEditorButton" $ noHtml
            li ! id "toggleEditorButton" $ noHtml
            li ! id "toggleErrorButton" $ noHtml
    packageEditorModal    -- Apparently, if we put this line
    openProjectModal      -- under this one. The open project modal doesn't work
    modalPromptPlaceholder "newDirectoryModal" "New Directory" "Choose a name for the new directory"

openProjectModal :: Perch
openProjectModal =
    div ! id "openProjectModal" ! atr "class" "modal modal-fixed-footer" $ do
        div ! atr "class" "modal-content" $ do
            h4 ("Open project" :: String)
            div $ do
                b ("Path to Stack project" :: String)
                div ! id "pathInput" $ noHtml
                p ! atr "class" "grey-text lighten-4" $ ("Path must be absolute, without ~ or environment variables." :: String)
                div ! id "creationDisplay" $ noHtml
                ul ! atr "class" "fs-tree collection" $ do
                  div ! atr "class" "collection-item row" $
                    li ! id "fsTree-tools" $ noHtml
                  div ! id "fsTree" $ noHtml

        div ! atr "class" "modal-footer" $
            div ! id "closeModalButton" $ noHtml

modalPromptPlaceholder :: String -> String -> String -> Perch
modalPromptPlaceholder id' htitle text = 
  div ! id id' ! atr "class" "modal" $ do
    div ! atr "class" "modal-content" $ do
      if (not . null) htitle then h4 htitle else noHtml
      div $ do
        if (not . null) text then label text else noHtml
        div ! atr "class" "input-container" $ noHtml

    div ! atr "class" "modal-footer" $
        div ! id (id' ++ "closeButton") $ noHtml

modalPrompt :: String -> (String -> Action) -> Action -> State -> Widget Action
modalPrompt id' inputAction buttonAction _ = inputWidget <|> closeButtonWidget
  where
    inputWidget = Ulmus.newWidget (id' ++ " .input-container") $ do
      _ <- getString Nothing
            `fire` OnKeyUp
      projPath <- liftIO $ getValueFromId ("#" ++ id' ++ " event input")
      return (inputAction projPath)

    closeButtonWidget = Ulmus.newWidget (id' ++ "closeButton") $ wlink buttonAction $
      a ! atr "class" "modal-action modal-close waves-effect btn-flat waves-purple" $
          i ! atr "class" "material-icons" $ ("input" :: String)

packageEditorModal :: Perch
packageEditorModal =
    div ! id "packageEditorModal" ! atr "class" "modal bottom-sheet" $ do
        div ! atr "class" "modal-content" $ do
            h4 ("Project settings" :: String)
            div $
                div ! id "packageTextArea" $ noHtml
        div ! atr "class" "modal-footer" $ do
            p ! atr "class" "red-text" $ ("Dependencies will be downloaded after confirming" :: String)
            div ! id "cancelPackageEditorButton" $ noHtml
            div ! id "closePackageEditorButton" $ noHtml

openProjectButton :: State -> Widget Action
openProjectButton _ = Ulmus.newWidget "openProjectButton" $ wlink OpenProject $
        a ! atr "class" "btn-floating purple darken-2 tooltipped" ! atr "data-position" "bottom" ! atr "data-tooltip" "New/Open" ! atr "data-delay" "50" $
            i ! atr "class" "material-icons" $ ("folder_open" :: String)

packageEditorButton :: State -> Widget Action
packageEditorButton _ = Ulmus.newWidget "packageEditorButton" $ wlink LoadPackageYaml $
        a ! atr "class" "btn-floating purple darken-2 tooltipped" ! atr "data-position" "bottom" ! atr "data-tooltip" "Project settings"  ! atr "data-delay" "50"$
            i ! atr "class" "material-icons" $ ("build" :: String)


compileButton :: State -> Widget Action
compileButton _ = Ulmus.newWidget "compileButton" $ wlink Compile $
    a ! atr "class" "btn-floating purple darken-2 tooltipped" ! atr "data-position" "bottom" ! atr "data-tooltip" "Compile [Ctrl+Return]" ! atr "data-delay" "50"$
        i ! atr "class" "material-icons" $ ("play_arrow" :: String)

toggleEditorButton :: State -> Widget Action
toggleEditorButton _ = Ulmus.newWidget "toggleEditorButton" $ wlink ToggleEditor $
    a ! atr "class" "btn-floating purple darken-2 tooltipped" ! atr "data-position" "bottom" ! atr "data-tooltip" "Toggle editor" ! atr "data-delay" "50"$
        i ! atr "class" "material-icons" $ ("remove_red_eye" :: String)

toggleErrorButton :: State -> Widget Action
toggleErrorButton _ = Ulmus.newWidget "toggleErrorButton" $ wlink ToggleError $
    a ! atr "class" "btn-floating purple darken-2 tooltipped" ! atr "data-position" "bottom" ! atr "data-tooltip" "Toggle error" ! atr "data-delay" "50"$
        i ! atr "class" "material-icons" $ ("error" :: String)


closeModalButton :: State -> Widget Action
closeModalButton _ = Ulmus.newWidget "closeModalButton" $ wlink LoadProject $
     a ! atr "class" "modal-action modal-close waves-effect btn-flat waves-purple" $
        i ! atr "class" "material-icons" $ ("input" :: String)

closePackageEditorButton :: State -> Widget Action
closePackageEditorButton _ = Ulmus.newWidget "closePackageEditorButton" $ wlink SavePackage $
    a ! atr "class" "modal-action modal-close waves-effect btn-flat waves-purple" $
        i ! atr "class" "material-icons" $ ("playlist_add_check" :: String)

cancelPackageEditorButton :: State -> Widget Action
cancelPackageEditorButton _ = Ulmus.newWidget "cancelPackageEditorButton" $ wlink ClosePackageModal $
    a ! atr "class" "modal-action modal-close waves-effect btn-flat waves-purple" $
        i ! atr "class" "material-icons" $ ("clear" :: String)

pathInput :: State -> Widget Action
pathInput state = Ulmus.newWidget "pathInput" $ do
    let pr = if lastProject state == ""
             then Nothing
             else Just $ lastProject state
    _ <- getString pr
            ! atr "placeholder" "/path/to/your/project"
            `fire` OnKeyUp
    projPath <- liftIO $ getValueFromId "#pathInput event input"
    return $ NewPath projPath

packageTextArea :: State -> Widget Action
packageTextArea _ = Ulmus.newWidget "packageTextArea" $ do
     _ <- getMultilineText "" ! atr "rows" "20" `fire` OnKeyUp
     newConfig <- liftIO $ getValueFromId "#packageTextArea event textarea"
     return $ NewPackage newConfig

creationDisplay :: State -> Widget ()
creationDisplay _ = Ulmus.newWidget "creationDisplay" $
    rawHtml $ p ! atr "class" "red-text" $ ("" :: String)

updateDisplays :: State -> Widget Action
updateDisplays = FileSystemTree.widget
