module HaskellDo.Compilation.View where

import           Control.Monad.IO.Class
import           Prelude                     hiding (div, id)

import           AxiomUtils
import           Foreign.Highlight
import           GHCJS.HPlay.View            hiding (atr)
import           Transient.Base
import qualified Ulmus

import           HaskellDo.Compilation.Types
import Foreign.Materialize

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
