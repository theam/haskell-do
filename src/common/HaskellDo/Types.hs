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
module HaskellDo.Types where

import qualified HaskellDo.SimpleMDE.Types as SimpleMDE
import qualified HaskellDo.Compilation.Types as Compilation
import qualified HaskellDo.Toolbar.Types as Toolbar

data AppState = AppState
  { simpleMDEState   :: SimpleMDE.State
  , compilationState :: Compilation.State
  , toolbarState     :: Toolbar.State
  } deriving (Read, Show)


data Action
  = SimpleMDEAction SimpleMDE.Action
  | ToolbarAction Toolbar.Action
  deriving (Read, Show)
