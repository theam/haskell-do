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
module Ulmus where

import Prelude hiding (div, id)
import Data.IORef
import Data.Typeable
import Control.Monad.IO.Class

import GHCJS.HPlay.View hiding (at, id)
import Transient.Base hiding (getState, setState)
import Transient.Move

import AxiomUtils

type Update action appState         = action -> appState -> Cloud appState
type View action appState           = appState -> Widget action
type UpdateDisplays action appState = appState -> Widget action

data AppConfig action appState = AppConfig
  { _update         :: Update action appState
  , _view           :: View action appState
  , _updateDisplays :: UpdateDisplays action appState
  , _initialState   :: appState
  , _port           :: Integer
  , _setup          :: IO ()
  }


initializeApp :: (Show appState, Show action, Read appState, Read action, Typeable appState, Typeable action)
              => AppConfig action appState
              -> IO ()
initializeApp (AppConfig update view updateDisplays initialAppState appPort setup) = do
    setup
    simpleWebApp appPort $ do
        step view
        loop (step updateDisplays)
  where
    step f = do
      currentState <- local $ getState initialAppState
      nextAction <- local (render $ f currentState)
      currentState' <- local $ getState initialAppState
      newState <- update nextAction currentState'
      local (setState newState)
    loop f = f >> loop f

widgetPlaceholder :: String -> Perch
widgetPlaceholder id' =
  div
    ! id id'
    $ noHtml

updateWidget :: String -> Widget () -> TransIO ()
updateWidget s f = render $ at ("#" ++ s) Insert f


withWidgets :: Widget a -> Perch -> Widget a
withWidgets widgets perch = rawHtml perch **> widgets


newWidget :: String -> Widget a -> Widget a
newWidget s = at ("#" ++ s) Insert

mapAction :: (actionA -> actionB) -> Widget actionA -> Widget actionB
mapAction actionConstructor widget = do
    action <- widget
    return $ actionConstructor action
---------------------------------------------  State manipulation -------------------------------

getState :: (Typeable appState) => appState -> TransIO appState
getState initialAppState = getRData <|> setAndReturn
 where
  setAndReturn = do
      setState initialAppState
      return initialAppState


setState :: (Typeable appState) => appState -> TransIO ()
setState = setRData

---------------------------------------------  State References in the TransIO monad ------------
newtype Ref a = Ref (IORef a)

-- | An state reference that can be updated (similar to STRef in the state monad)
--
-- Initialized the first time it is set.
setRData :: Typeable a => a -> TransIO ()
setRData x = do
     Ref ref <- getSData
     liftIO $ atomicModifyIORef ref $ const (x,())
   <|> do
     ref <- liftIO (newIORef x)
     setData $ Ref ref

getRData :: Typeable a => TransIO a
getRData= do
    Ref ref <- getSData
    liftIO $ readIORef ref
