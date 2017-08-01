
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
module HaskellDo.View where

import Prelude hiding (id, div)

import GHCJS.HPlay.View
import Transient.Internals ((**>))
import qualified Ulmus

import HaskellDo.Types
import qualified HaskellDo.Materialize.View as Materialize
import qualified HaskellDo.CodeMirror.View as CodeMirror
import qualified HaskellDo.Compilation.View as Compilation
import qualified HaskellDo.Toolbar.View as Toolbar
import qualified HaskellDo.Toolbar.Types as Toolbar
import qualified HaskellDo.Toolbar.FileSystemTree as FileSystemTree

view :: AppState -> Widget Action
view appState = Ulmus.withWidgets (widgets appState) $
  div ! atr "class" "editor-container" $ do
      Materialize.row $ do
            Materialize.col "s" 6 $
                Ulmus.widgetPlaceholder "editor"
            Materialize.col "s" 6 ! id "outputdiv" $ do
                Ulmus.widgetPlaceholder "outputDisplay"
                loaderOverlay
      Materialize.row $
            Materialize.col "s" 12 $ div ! atr "class" "error-placeholder" $ noHtml
      Ulmus.widgetPlaceholder "errorDisplay"

loaderOverlay :: Perch
loaderOverlay =
    div ! atr "class" "dimmedBackground" $
        div ! atr "class" "loader-align center-align" $
            div ! atr "class" "loader-align-inner" $ do
                div ! atr "class" "preloader-wrapper big active" $
                    div ! atr "class" "spinner-layer spinner-blue-only" $ do
                        div ! atr "class" "circle-clipper left" $
                            div ! atr "class" "circle" $ noHtml
                        div ! atr "class" "gap-patch" $
                            div ! atr "class" "circle" $ noHtml
                        div ! atr "class" "circle-clipper right" $
                            div ! atr "class" "circle" $ noHtml
                p ! atr "class" "grey-text center-align" ! atr "id" "dependencyMessage" $ ("Downloading dependencies" :: String)
widgets :: AppState -> Widget Action
widgets state = do
    Toolbar.toolbar
    Toolbar.creationDisplay (toolbarState state)
    showDisplays state
    codeMirrorWidget
    **> packageTextAreaWidget
    **> openProjectButtonWidget
    **> packageEditorButtonWidget
    **> toggleEditorButtonWidget
    **> toggleErrorButtonWidget
    **> compileButtonWidget
    **> pathInputWidget
    **> closeModalButtonWidget
    **> closePackageEditorButtonWidget
    **> cancelPackageEditorButtonWidget
    **> fsTreeWidget
    **> modalPrompt "newDirectoryModal" Toolbar.NewDirectory Toolbar.CreateNewDirectory
  where
    modalPrompt id' inputAction buttonAction = Ulmus.mapAction ToolbarAction $
        Toolbar.modalPrompt id' inputAction buttonAction (toolbarState state)

    codeMirrorWidget = Ulmus.newWidget "editor" $
        Ulmus.mapAction CodeMirrorAction $
            CodeMirror.view $ codeMirrorState state

    openProjectButtonWidget = Ulmus.mapAction ToolbarAction $
        Toolbar.openProjectButton (toolbarState state)

    packageEditorButtonWidget = Ulmus.mapAction ToolbarAction $
        Toolbar.packageEditorButton (toolbarState state)

    compileButtonWidget = Ulmus.mapAction ToolbarAction $
        Toolbar.compileButton (toolbarState state)

    toggleEditorButtonWidget = Ulmus.mapAction ToolbarAction $
        Toolbar.toggleEditorButton (toolbarState state)

    toggleErrorButtonWidget = Ulmus.mapAction ToolbarAction $
        Toolbar.toggleErrorButton (toolbarState state)

    pathInputWidget = Ulmus.mapAction ToolbarAction $
        Toolbar.pathInput (toolbarState state)

    packageTextAreaWidget = Ulmus.mapAction ToolbarAction $
        Toolbar.packageTextArea (toolbarState state)

    fsTreeWidget = Ulmus.mapAction ToolbarAction $
        FileSystemTree.widget (toolbarState state)

    closeModalButtonWidget = Ulmus.mapAction ToolbarAction $
        Toolbar.closeModalButton (toolbarState state)

    closePackageEditorButtonWidget = Ulmus.mapAction ToolbarAction $
        Toolbar.closePackageEditorButton (toolbarState state)

    cancelPackageEditorButtonWidget = Ulmus.mapAction ToolbarAction $
        Toolbar.cancelPackageEditorButton (toolbarState state)


showDisplays :: AppState -> Widget ()
showDisplays state = do
    Ulmus.newWidget "outputDisplay" $ Compilation.outputDisplay (compilationState state)
    Ulmus.newWidget "errorDisplay" $ Compilation.errorDisplay (compilationState state)

updateDisplays :: AppState -> Widget Action
updateDisplays state = do
    Compilation.updateDisplays (compilationState state)
    Ulmus.mapAction ToolbarAction $
      Toolbar.updateDisplays (toolbarState state)
