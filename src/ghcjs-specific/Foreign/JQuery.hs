module Foreign.JQuery where

import GHCJS.Types
import Data.JSString

foreign import javascript unsafe "$($1).val()"
  js_getValueFromId :: JSString -> IO JSString

foreign import javascript unsafe "$($1).val($2);"
  js_setValueForId :: JSString -> JSString -> IO ()

foreign import javascript unsafe "$($1).html($2);"
  js_setHtmlForId :: JSString -> JSString -> IO ()

foreign import javascript unsafe "$($1).show();"
  js_show :: JSString -> IO ()

foreign import javascript unsafe "$($1).hide();"
  js_hide :: JSString -> IO ()

foreign import javascript unsafe "$($1).effect('shake');"
  js_shake :: JSString -> IO ()

getValueFromId :: String -> IO String
getValueFromId s = do
  r <- js_getValueFromId $ pack s
  return $ unpack r

setValueForId :: String -> String -> IO ()
setValueForId id' s = js_setValueForId (pack id') (pack s)

setHtmlForId :: String -> String -> IO ()
setHtmlForId id' s = js_setHtmlForId (pack id') (pack s)

show :: String -> IO ()
show = js_show . pack

hide :: String -> IO ()
hide = js_hide . pack

shake :: String -> IO ()
shake = js_shake . pack
