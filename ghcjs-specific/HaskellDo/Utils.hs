module HaskellDo.Utils where

import BasicPrelude
import GHCJS.HPlay.View hiding (id)
import GHCJS.Types

setContents :: Perch -> String -> Perch
setContents element content = element `setHtml` (fromString content :: JSString)
