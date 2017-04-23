module HaskellDo.GUI.SimpleMDE
  ( module HaskellDo.GUI.SimpleMDE.Common
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

import HaskellDo.GUI.SimpleMDE.Common
