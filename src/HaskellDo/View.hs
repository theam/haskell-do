
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

import BasicPrelude hiding (id)

import GHCJS.HPlay.View
import Transient.Base

import qualified Ulmus
import HaskellDo.Types
import HaskellDo.GUI.External.SimpleMDE

view :: AppState -> Widget Action
view appState = do
      messageDisplay appState
      wbutton Compile "Go"
      <|> editor appState


editor :: AppState -> Widget Action
editor _ = do
    newMsg <- simpleMDE
    return $ EditorChanged newMsg

updateDisplays :: AppState -> TransIO ()
updateDisplays appState = do
  Ulmus.updateWidget "messageDisplay" (messageDisplay appState)


messageDisplay :: AppState -> Widget ()
messageDisplay appState = rawHtml $
  h1 ! id "messageDisplay"
           $ "Code: " ++ appStateMessage appState
