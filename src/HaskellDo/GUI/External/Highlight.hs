module HaskellDo.GUI.External.Highlight where

import BasicPrelude

#ifdef ghcjs_HOST_OS
foreign import javascript unsafe "$('.haskell').each(function(i, block){ hljs.highlightBlock(block);});"
    highlightCode :: IO ()
#else
highlightCode :: IO ()
highlightCode = return ()
#endif
