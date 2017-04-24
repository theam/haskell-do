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
  { appStateMessage = ""
  }

update :: Action -> AppState -> Cloud AppState
update (EditorChanged newMsg) appState = do
    parsed <- atRemote $ Compilation.compile newMsg
    local $ liftIO $ SimpleMDE.setRendered parsed
    return $ appState { appStateMessage = newMsg }
