
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
import Control.Monad.IO.Class

import Transient.Base
import GHCJS.HPlay.View
import qualified Ulmus

import HaskellDo.Types
import qualified HaskellDo.Materialize.View as Materialize
import qualified HaskellDo.SimpleMDE.View as SimpleMDE
import qualified HaskellDo.Compilation.View as Compilation
import qualified HaskellDo.Toolbar.View as Toolbar

view :: AppState -> Widget Action
view appState = Ulmus.withWidgets (widgets appState) $ do
    Ulmus.widgetPlaceholder "debug"
    div ! atr "class" "editor-container" $ do
        Materialize.row $ do
            Materialize.col "s" 6 $ do
                Ulmus.widgetPlaceholder "editor"
            Materialize.col "s" 6 $ do
                Ulmus.widgetPlaceholder "outputDisplay"
        Materialize.row $ do
            Materialize.col "s" 12 $ do
                Ulmus.widgetPlaceholder "errorDisplay"

widgets :: AppState -> Widget Action
widgets state = do
    Toolbar.toolbar
    showDisplays state
    simpleMDEWidget
    <|> openProjectButtonWidget
    <|> compileButtonWidget
    <|> pathInputWidget
  where
    simpleMDEWidget = Ulmus.newWidget "editor" $
        Ulmus.mapAction SimpleMDEAction $
            SimpleMDE.view $ simpleMDEState state
    openProjectButtonWidget = Ulmus.mapAction ToolbarAction $
        Toolbar.openProjectButton (toolbarState state)
    compileButtonWidget = Ulmus.mapAction ToolbarAction $
        Toolbar.compileButton (toolbarState state)
    pathInputWidget = Ulmus.mapAction ToolbarAction $
        Toolbar.pathInput (toolbarState state)

showDisplays :: AppState -> Widget ()
showDisplays state = do
    Ulmus.newWidget "outputDisplay" $ Compilation.outputDisplay (compilationState state)
    Ulmus.newWidget "errorDisplay" $ Compilation.errorDisplay (compilationState state)
    Ulmus.newWidget "debug" $ rawHtml $ p (show state :: String)

updateDisplays :: AppState -> TransIO ()
updateDisplays state = do
    Compilation.updateDisplays (compilationState state)
    Ulmus.updateWidget "debug" $ rawHtml $ p (show state :: String)
