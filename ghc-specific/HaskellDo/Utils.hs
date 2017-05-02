module HaskellDo.Utils where

import BasicPrelude
import GHCJS.HPlay.View hiding (id)

setContents :: Perch -> String -> Perch
setContents element _ = element
