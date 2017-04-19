module HaskellDo.GUI.SimpleMDE 
  ( simpleMDEFromId
#ifdef ghcjs_HOST_OS
  , module HaskellDo.GUI.SimpleMDE.JavascriptInternals
#else
  , module HaskellDo.GUI.SimpleMDE.ServerInternals
#endif
  )
where

#ifdef ghcjs_HOST_OS
import HaskellDo.GUI.SimpleMDE.JavascriptInternals
#else
import HaskellDo.GUI.SimpleMDE.ServerInternals
#endif

import BasicPrelude

import HaskellDo.GUI.SimpleMDE.Common

simpleMDEFromId :: String -> IO SimpleMDE
simpleMDEFromId eid = simpleMDE (EditorConfig eid)
