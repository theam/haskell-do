module HaskellDo.GUI.Utils where

import BasicPrelude
import GHCJS.HPlay.View hiding (id)
#ifdef ghcjs_HOST_OS
import GHCJS.Types
#endif

setContents :: Perch -> String -> Perch
#ifdef ghcjs_HOST_OS
setContents element content = element `setHtml` (fromString content :: JSString)
#else
setContents element _ = element
#endif
