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
module HaskellDo.Compilation.View where

import           Control.Monad.IO.Class
import           Prelude                     hiding (div, id)

import           AxiomUtils
import           Foreign.Highlight
import           GHCJS.HPlay.View            hiding (atr)
import           Transient.Base
import qualified Ulmus

import           HaskellDo.Compilation.Types

outputDisplay :: State -> Widget ()
outputDisplay state = rawHtml $
    div noHtml `setContents` compiledOutput state

errorDisplay :: State -> Widget ()
errorDisplay state
 | null (compilationError state) = return ()
 | otherwise = rawHtml $
    div
        ! atr "class" "card-panel red darken-1 white-text"
        ! atr "role" "alert"
        $ compilationError state

updateDisplays :: State -> TransIO ()
updateDisplays state = do
  Ulmus.updateWidget "outputDisplay" (outputDisplay state)
  Ulmus.updateWidget "errorDisplay" (errorDisplay state)
  liftIO highlightCode
