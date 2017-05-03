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

import Transient.Move

import HaskellDo.Types
import qualified HaskellDo.SimpleMDE.State as SimpleMDE
import qualified HaskellDo.SimpleMDE.Types as SimpleMDE
import qualified HaskellDo.Compilation.State as Compilation
import qualified HaskellDo.Compilation.Types as Compilation

initialAppState :: AppState
initialAppState = AppState
  { simpleMDEState = SimpleMDE.initialState
  , compilationState = Compilation.initialState
  }

update :: Action -> AppState -> Cloud AppState
update (SimpleMDEAction action) appState = do
    newSimpleMDEState <- SimpleMDE.update action (simpleMDEState appState)
    let newContent = SimpleMDE.content newSimpleMDEState
    atRemote $ Compilation.update
        (Compilation.WriteWorkingFile newContent)
        (compilationState appState)
    newCompilationState <- atRemote $ Compilation.update
        Compilation.Compile
        (compilationState appState)
    return appState
        { simpleMDEState = newSimpleMDEState
        , compilationState = newCompilationState
        }
