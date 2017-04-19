{-
 - src\HaskellDo.hs
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

import BasicPrelude hiding (id, div, empty)

import GHCJS.HPlay.View hiding (map, option,input)
import Transient.Base
import Transient.Move

import qualified Ulmus 

data AppState = AppState
  { appStateMessage :: String
  } deriving (Read, Show, Eq)


data Action
  = EditorChanged String
  deriving (Read, Show)


initialAppState :: AppState
initialAppState = AppState
  { appStateMessage = ""
  }


-- | Executes Haskell.do in designated 'port'
run :: IO ()
run = Ulmus.initializeApp Ulmus.AppConfig
  { Ulmus._update         = update
  , Ulmus._view           = view
  , Ulmus._updateDisplays = updateDisplays
  , Ulmus._initialState   = initialAppState
  , Ulmus._port           = 8080
  }


view :: AppState -> Widget Action
view appState = do
      messageDisplay appState
      editor appState


editor :: AppState -> Widget Action
editor _ = do
  newMsg <- getMultilineText "" `fire` OnKeyDown
  return $ EditorChanged newMsg


update :: Action -> AppState -> Cloud AppState
update (EditorChanged newMsg) appState = return $ appState { appStateMessage = newMsg }


updateDisplays :: AppState -> TransIO ()
updateDisplays appState = do
  Ulmus.updateWidget "messageDisplay" (messageDisplay appState)


messageDisplay :: AppState -> Widget ()
messageDisplay appState = rawHtml $ 
  h2 ! id "messageDisplay"
     $ "Code: " ++ appStateMessage appState

