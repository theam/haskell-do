module External.Highlight where

import BasicPrelude

foreign import javascript unsafe "$('.haskell').each(function(i, block){ hljs.highlightBlock(block);});"
    highlightCode :: IO ()
