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
module HaskellDo.State where

import BasicPrelude

import Transient.Move

import HaskellDo.Types
import qualified HaskellDo.Core.Compilation as Compilation
import qualified HaskellDo.GUI.External.SimpleMDE as SimpleMDE

initialAppState :: AppState
initialAppState = AppState
  { editorCode     = ""
  , codeHtmlOutput = "Not compiled yet."
  }

update :: Action -> AppState -> Cloud AppState
update (EditorChanged newMsg) appState = do
    let newState = appState { editorCode = newMsg }
    return newState

update Compile appState = do
    local $ liftIO $ SimpleMDE.setRendered "Compiling..."
    parsed <- atRemote $ Compilation.compile (editorCode appState)
    local $ liftIO $ SimpleMDE.setRendered parsed
    return $ appState { codeHtmlOutput = parsed }
