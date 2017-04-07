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
{- Language NoImplicitPrelude #-}
{-# Language CPP #-}
{-# Language NoMonomorphismRestriction #-}
{-# Language IncoherentInstances #-}

module HaskellDo 
  ( ServiceType(..)
  , run
  ) where

import Flow

import Transient.Internals



import GHCJS.HPlay.Cell
import GHCJS.HPlay.View hiding (map, option,input)


import Transient.Move
import Transient.EVars
import Transient.Indeterminism

import Control.Applicative
import qualified Data.Vector as V
import qualified Data.Map as M
import Transient.MapReduce
import Control.Monad.IO.Class
import Data.String
import qualified Data.Text as T

#ifdef ghcjs_HOST_OS
import qualified Data.JSString as JS hiding (span,empty,strip,words)
#endif

import Data.Typeable


-- | Defines how to run Haskell.do through 'run'
data ServiceType
  = ComputationNode
  | DesktopApp
  | WebApp

-- | Executes Haskell.do as defined in 'ServiceType', in designated 'port'
run :: Integer -> IO ()
run port = simpleWebApp port (app WebApp)

app :: ServiceType -> Cloud ()
app ComputationNode = undefined             
app DesktopApp = undefined             
app WebApp = runWebApp

fs= fromString

data Action
  = ButtonClicked String
  deriving (Read, Show)

data AppState = AppState
  { message :: String
  } deriving (Read, Show)

view :: AppState -> Cloud Action
view appState = local . render $ do
  rawHtml (h1 $ message appState)
  wbutton (ButtonClicked "Hello world") (fromString "Click me")

update :: Action -> AppState -> Cloud AppState
update (ButtonClicked s) _ = return $ AppState s

runWebApp :: Cloud ()
runWebApp = onBrowser $ do
  let initialState = AppState ""

  action <- view initialState

  newState <- atRemote $ do
         lliftIO $ print "Clicked link"
         update action initialState

  view newState

  empty
  
