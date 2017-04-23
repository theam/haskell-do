module HaskellDo.GUI.SimpleMDE
  ( simpleMDEFromId
#ifdef ghcjs_HOST_OS
  , module HaskellDo.GUI.SimpleMDE.JavascriptInternals
#else
  , module HaskellDo.GUI.SimpleMDE.ServerInternals
#endif
  , module HaskellDo.GUI.SimpleMDE.Common
  )
where

#ifdef ghcjs_HOST_OS
import HaskellDo.GUI.SimpleMDE.JavascriptInternals
#else
import HaskellDo.GUI.SimpleMDE.ServerInternals
#endif

import BasicPrelude

import GHCJS.HPlay.View hiding (map, option,input)

import HaskellDo.GUI.SimpleMDE.Common

simpleMDEFromId :: String -> Widget String
simpleMDEFromId eid = simpleMDE (EditorConfig eid)
