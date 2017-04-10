{-# Language NoImplicitPrelude #-}
{-# Language CPP #-}
{-# Language NoMonomorphismRestriction #-}
{-# Language IncoherentInstances #-}
{-# Language OverloadedStrings #-}
module Ulmus where

import BasicPrelude hiding (id, div, empty)

import GHCJS.HPlay.View hiding (map, option,input)


import Transient.Move


data AppConfig appState action = AppConfig
  { viewFunction :: appState -> Widget action
  , updateFunction :: action -> appState -> Cloud appState
  , initialAppState :: appState
  , executionPort :: Integer
  }

startApp :: (Show action, Read action, Typeable action, Read appState, Show appState, Typeable appState)
         => AppConfig appState action
         -> IO ()
startApp cfg = simpleWebApp (executionPort cfg) $ do
  local . render . rawHtml $ div ! id divName $ noHtml
  loop (initialAppState cfg)
 where
  divName = "applicationDiv"
  view state = local . render $ at (fromString $ "#" ++ divName) Insert $ viewFunction cfg state
  loop state = onBrowser $ do
    nextAction <- view state
    newState <- atRemote $ updateFunction cfg nextAction state
    loop newState

-- test