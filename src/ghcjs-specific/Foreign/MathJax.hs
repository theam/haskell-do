module Foreign.MathJax where

import GHCJS.Types
import Data.JSString

foreign import javascript unsafe "MathJax.Hub.Queue(['Typeset', MathJax.Hub, $1]);"
    js_typeset :: JSString -> IO ()

typeset :: String -> IO ()
typeset = js_typeset . pack

