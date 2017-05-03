module HaskellDo.Displays where

import Prelude hiding (div, id)
import Control.Monad.IO.Class

import GHCJS.HPlay.View
import Transient.Base
import HaskellDo.Types
import qualified Ulmus
import AxiomUtils
import Foreign.Highlight

outputDisplay :: AppState -> Widget ()
outputDisplay appState = Ulmus.newWidget "outputDisplay" $ rawHtml $ do
  div noHtml `setContents` codeHtmlOutput appState

errorDisplay :: AppState -> Widget ()
errorDisplay appState = Ulmus.newWidget "errorDisplay" $ rawHtml $
  div noHtml `setContents` compilationError appState

updateDisplays :: AppState -> TransIO ()
updateDisplays appState = do
  Ulmus.updateWidget "outputDisplay" (outputDisplay appState)
  Ulmus.updateWidget "errorDisplay" (errorDisplay appState)
  liftIO highlightCode
