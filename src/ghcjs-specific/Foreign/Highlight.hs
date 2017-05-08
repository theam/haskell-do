module Foreign.Highlight where

foreign import javascript unsafe "$('.haskell').each(function(i, block){ hljs.highlightBlock(block);});"
    highlightCode :: IO ()
