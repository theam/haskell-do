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
import Flow

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
      , initialAppState = AppState 0 ""
      , executionPort = 8080
      }

data Action
  = ButtonClicked
  | TextAreaChanged String
  deriving (Read, Show)

data AppState = AppState
  { message :: Int
  , appStateCode :: String
  } deriving (Read, Show)

view :: AppState -> Widget Action
view appState = do
  numberDisplay appState
  codeDisplay appState
  codeEditor appState
  <|> myButton

numberDisplay :: AppState -> Widget ()
numberDisplay appState = message appState
                       |> show
                       |> h1
                       |> rawHtml

codeDisplay :: AppState -> Widget ()
codeDisplay appState = "Code: " ++ appStateCode appState
                     |> h2
                     |> rawHtml

codeEditor :: AppState -> Widget Action
codeEditor appState = do
  let c = appStateCode appState
  code <- getMultilineText (fromString c) `fire` OnChange
  return $ TextAreaChanged code
  
myButton :: Widget Action
myButton = wbutton ButtonClicked (fromString "Click me")

update :: Action -> AppState -> Cloud AppState
update ButtonClicked (AppState n c) = return $ AppState (n+1) c
update (TextAreaChanged c) appState = return $ appState { appStateCode = c }

