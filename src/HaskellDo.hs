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
{- Language IncoherentInstances #-}
{-# Language OverloadedStrings #-}

module HaskellDo
  ( run
  ) where

import BasicPrelude hiding (id, div, empty)
import Flow

import GHCJS.HPlay.View hiding (map, option,input)
import Transient.Base
import Transient.Move
import Control.Monad.IO.Class
import Data.IORef
import Data.Typeable

data AppState = AppState
  { appStateMessage :: String
  , firstRun :: Bool
  } deriving (Read, Show, Eq)

data Action
  = EditorChanged String
  deriving (Read, Show)

initialAppState = AppState "" True


-- | Executes Haskell.do in designated 'port'
run :: IO ()
run = simpleWebApp 8080 $ initializeApp update view display 

view :: AppState -> Widget Action
view appState = do
      displayPlaceholder "messageDisplay"
      editor appState

editor :: AppState -> Widget Action
editor appState = do
  newMsg <- getMultilineText "" `fire` OnKeyDown
  return $ EditorChanged newMsg

update :: Action -> AppState -> Cloud AppState
update (EditorChanged newMsg) appState = return $ appState { appStateMessage = newMsg }

display :: AppState -> TransIO ()
display appState = do
  render $ at "#messageDisplay" Insert $ do
         rawHtml $ h2 $ appStateMessage appState

---------------------------------------------  State manipulation -------------------------------

getState :: TransIO AppState
getState 
  = getRData
  <|> (setRData initialAppState >> return initialAppState)

setState :: AppState -> TransIO ()
setState = setRData



---------------------------------------- TODO: Internal functions, extract into module-----------

initializeApp :: (Action -> AppState -> Cloud AppState)   -- update function
              -> (AppState -> Widget Action)              -- view function
              -> (AppState -> TransIO ())                 -- display function
              -> Cloud ()
initializeApp update view display = do
  currentState <- local getState
  nextAction <- local (render $ view currentState)
  newState <- update nextAction currentState
  local (setState newState)
  renderDisplay display

displayPlaceholder :: String -> Widget ()
displayPlaceholder id' = rawHtml $ 
  div 
    ! id (fromString id')
    $ noHtml

renderDisplay :: (AppState -> TransIO ()) -> Cloud ()
renderDisplay f = do
  state <- local getState
  local $ f state


---------------------------------------------  State References in the TransIO monad ------------
newtype Ref a = Ref (IORef a)

-- | An state reference that can be updated (similar to STRef in the state monad)
--
-- Initialized the first time it is set.
setRData:: Typeable a => a -> TransIO ()
setRData x= do
     Ref ref <- getSData
     liftIO $ atomicModifyIORef ref $ const (x,())
   <|> do
     ref <- liftIO (newIORef x)
     setData $ Ref ref

getRData :: Typeable a => TransIO a
getRData= do
    Ref ref <- getSData
    liftIO $ readIORef ref

