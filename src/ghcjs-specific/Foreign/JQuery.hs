module Foreign.JQuery where

import GHCJS.Types
import Data.JSString

foreign import javascript unsafe "$($1).val()"
  js_getValueFromId :: JSString -> IO JSString

getValueFromId :: String -> IO String
getValueFromId s = do
  r <- js_getValueFromId $ pack s
  return $ unpack r
