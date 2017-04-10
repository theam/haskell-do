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
{-# Language NoImplicitPrelude #-}
{-# Language CPP #-}
{-# Language NoMonomorphismRestriction #-}
{-# Language IncoherentInstances #-}
{-# Language OverloadedStrings #-}

module HaskellDo
  ( run
  ) where

import BasicPrelude hiding (id, div, empty)

import GHCJS.HPlay.View hiding (map, option,input)
import Transient.Move

import Ulmus


-- | Executes Haskell.do in designated 'port'
run :: IO ()
run = startApp config
 where
  config = AppConfig
      { viewFunction = view
      , updateFunction = update
      , initialAppState = AppState 0
      , executionPort = 8080
      }

data Action
  = ButtonClicked
  deriving (Read, Show)

data AppState = AppState
  { message :: Int
  } deriving (Read, Show)


view :: AppState -> Widget Action
view appState = do
  numberDisplay $ message appState
  wbutton ButtonClicked (fromString "Click me")

numberDisplay :: Int -> Widget ()
numberDisplay = rawHtml . h1 . show

update :: Action -> AppState -> Cloud AppState
update ButtonClicked (AppState n) = return $ AppState (n+1)

