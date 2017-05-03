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
module HaskellDo
  ( run
  ) where

import qualified Ulmus

import HaskellDo.View
import HaskellDo.Displays
import HaskellDo.State
import qualified HaskellDo.SimpleMDE.View as SimpleMDE
import qualified HaskellDo.Style.View as Style

initializeHeaders :: IO ()
initializeHeaders = do
    SimpleMDE.initialize
    Style.initialize

-- | Executes Haskell.do in designated 'port'
run :: IO ()
run = Ulmus.initializeApp Ulmus.AppConfig
  { Ulmus._update         = update
  , Ulmus._view           = view
  , Ulmus._updateDisplays = updateDisplays
  , Ulmus._initialState   = initialAppState
  , Ulmus._port           = 8080
  , Ulmus._setup          = initializeHeaders
  }
