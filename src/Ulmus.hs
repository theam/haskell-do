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

import BasicPrelude hiding (id, div, empty)

import GHCJS.HPlay.View hiding (map, option,input)


import Transient.Base
import Transient.Move
import Data.IORef

type Update action appState  = action -> appState -> Cloud appState
type View action appState    = appState -> Widget action
type UpdateDisplays appState = appState -> TransIO ()

data AppConfig action appState = AppConfig
  { _update         :: Update action appState
  , _view           :: View action appState
  , _updateDisplays :: UpdateDisplays appState
  , _initialState   :: appState
  , _port           :: Integer
  , _setup          :: IO ()
  }


initializeApp :: (Show appState, Show action, Read appState, Read action, Typeable appState, Typeable action)
              => AppConfig action appState
              -> IO ()
initializeApp (AppConfig update view updateDisplays initialAppState port setup) = do
    setup
    simpleWebApp port $ do
        currentState <- local $ getState initialAppState
        nextAction <- local (render $ view currentState)
        currentState <- local $ getState initialAppState
        local $ liftIO $ print $  "Updating with: " ++ show nextAction ++ " and " ++ show currentState
        newState <- update nextAction currentState
        local (setState newState)
        renderDisplay initialAppState updateDisplays


displayPlaceholder :: String -> Widget ()
displayPlaceholder id' = rawHtml $
  div
    ! id (fromString id')
    $ noHtml


renderDisplay :: (Show appState, Read appState, Typeable appState)
              => appState
              -> UpdateDisplays appState
              -> Cloud ()
renderDisplay initialAppState f = do
  state <- local $ getState initialAppState
  local $ f state


updateWidget :: String -> Widget () -> TransIO ()
updateWidget s f = render $ at ("#" ++ fromString s) Insert f
---------------------------------------------  State manipulation -------------------------------

getState :: (Typeable appState, Show appState) => appState -> TransIO appState
getState initialAppState = (do
    liftIO $ putStrLn "Getting data"
    x <- getRData
    liftIO $ print x
    liftIO $ putStrLn "\n-----------------------"
    return x) <|> setAndReturn
 where
  setAndReturn = do
      liftIO $ putStrLn $ "Data not set, setting initial state"
      setState initialAppState
      return initialAppState


setState :: (Show appState, Typeable appState) => appState -> TransIO ()
setState st = do
    liftIO $ print $ "Setting data " ++ show st
    setRData st

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
