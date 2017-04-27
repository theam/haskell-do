
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
module HaskellDo.View where

import BasicPrelude hiding (id, div)

import GHCJS.HPlay.View
import Transient.Base

import qualified Ulmus
import HaskellDo.Types
import HaskellDo.GUI.External.SimpleMDE
import qualified HaskellDo.GUI.External.Bootstrap as Bootstrap

#ifdef ghcjs_HOST_OS
import GHCJS.Types
#endif

view :: AppState -> Widget Action
view appState = do
      editor appState


editor :: AppState -> Widget Action
editor appState = do
    newMsg <- Bootstrap.container <<<
        ((Bootstrap.row <<<
            ((Bootstrap.col "sm" 6 <<< simpleMDE)
            <** (Bootstrap.col "sm" 6 <<< outputDisplay appState)))
        <** (Bootstrap.row <<<
                (Bootstrap.col "sm" 12 <<< errorDisplay appState)))
    return $ EditorChanged newMsg

outputDisplay :: AppState -> Widget ()
outputDisplay appState = rawHtml $ setContents (div ! id "outputDisplay" $ noHtml)
  where
#ifdef ghcjs_HOST_OS
    setContents f = f `setHtml` (fromString $ codeHtmlOutput appState :: JSString)
#else
    setContents f = f
#endif

errorDisplay :: AppState -> Widget ()
errorDisplay appState = rawHtml $ setContents (div ! id "errorDisplay" $ noHtml)
  where
#ifdef ghcjs_HOST_OS
    setContents f = f `setHtml` (fromString $ compilationError appState :: JSString)
#else
    setContents f = f
#endif

updateDisplays :: AppState -> TransIO ()
updateDisplays appState = do
  Ulmus.updateWidget "outputDisplay" (outputDisplay appState)
  Ulmus.updateWidget "errorDisplay" (errorDisplay appState)
  liftIO $ highlightCode

#ifdef ghcjs_HOST_OS
foreign import javascript unsafe "$('.haskell').each(function(i, block){ hljs.highlightBlock(block);});"
    highlightCode :: IO ()
#else
highlightCode :: IO ()
highlightCode = return ()
#endif
