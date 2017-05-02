
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

import BasicPrelude hiding (id, div)

import GHCJS.HPlay.View
import Transient.Base

import qualified Ulmus
import HaskellDo.Types
import External.SimpleMDE
import External.Highlight
import HaskellDo.Utils
import qualified External.Materialize as Materialize

view :: AppState -> Widget Action
view appState = editor appState

editor :: AppState -> Widget Action
editor appState = do
    newMsg <- Materialize.container <<<
        ((Materialize.row <<<
            ((Materialize.col "s" 6 <<< simpleMDE)
            <** (Materialize.col "s" 6 <<< outputDisplay appState)))
        <** (Materialize.row <<<
                (Materialize.col "s" 12 <<< errorDisplay appState)))
    return $ EditorChanged newMsg

outputDisplay :: AppState -> Widget ()
outputDisplay appState = rawHtml $
  (div ! id "outputDisplay" $ noHtml) `setContents` codeHtmlOutput appState

errorDisplay :: AppState -> Widget ()
errorDisplay appState = rawHtml $
  (div ! id "errorDisplay" $ noHtml) `setContents` (compilationError appState)

updateDisplays :: AppState -> TransIO ()
updateDisplays appState = do
  Ulmus.updateWidget "outputDisplay" (outputDisplay appState)
  Ulmus.updateWidget "errorDisplay" (errorDisplay appState)
  liftIO highlightCode
