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
import qualified HaskellDo.Compilation as Compilation
import qualified HaskellDo.SimpleMDE.State as SimpleMDE
import qualified HaskellDo.SimpleMDE.Types as SimpleMDE

initialAppState :: AppState
initialAppState = AppState
  { simpleMDEState = SimpleMDE.initialState
  , codeHtmlOutput = ""
  , compilationError = ""
  , projectPath = "/home/nick/Documents/haskell-do-test"
  }

update :: Action -> AppState -> Cloud AppState
update (SimpleMDEAction action) appState = do
    {-
    FIXME:
    rts.js:18932 Uncaught TypeError: Cannot read property 'f' of null
        at h$e (rts.js:18932) at h$baseZCGHCziShowzishowLitString_e (out.js:14312)
        at h$runThreadSlice (rts.js:8253)
        at h$runSyncAction (rts.js:8414)
        at HTMLUnknownElement.h$runSync (rts.js:8385)
        at HTMLUnknownElement.c (rts.js:4899)
    -}
    newSimpleMDEState <- SimpleMDE.update action (simpleMDEState appState)
    parsed <- atRemote $ Compilation.compile (projectPath appState) (SimpleMDE.content newSimpleMDEState)
    case parsed of
        Left err -> return $ appState { simpleMDEState = newSimpleMDEState, compilationError = err }
        Right out -> return $ appState { simpleMDEState = newSimpleMDEState, compilationError = "", codeHtmlOutput = out }
