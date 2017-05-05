module Foreign.JQuery where

import GHCJS.Types
import Data.JSString

foreign import javascript unsafe "$($1).val()"
  js_getValueFromId :: JSString -> IO JSString

foreign import javascript unsafe "$($1).show();"
  js_show :: JSString -> IO ()

foreign import javascript unsafe "$($1).hide();"
  js_hide :: JSString -> IO ()

getValueFromId :: String -> IO String
getValueFromId s = do
  r <- js_getValueFromId $ pack s
  return $ unpack r

show :: String -> IO ()
show = js_show . pack

hide :: String -> IO ()
hide = js_hide . pack
