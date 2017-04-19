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
  }


initializeApp :: (Show appState, Show action, Read appState, Read action, Typeable appState, Typeable action)
              => AppConfig action appState 
              -> IO ()
initializeApp (AppConfig update view updateDisplays initialAppState port) = simpleWebApp port $ do
  currentState <- local $ getState initialAppState
  nextAction <- local (render $ view currentState)
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


