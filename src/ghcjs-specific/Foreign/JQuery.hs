{-
 - Copyright (c) 2017 The Agile Monkeys S.L. <hackers@theam.io>
 -
 - Licensed under the Apache License, Version 2.0 (the "License");
 - you may not use this file except in compliance with the License.
 - You may obtain a copy of the License at
 -
 -     http://www.apache.org/licenses/LICENSE-2.0
 -
 - Unless required by applicable law or agreed to in writing, software
 - distributed under the License is distributed on an "AS IS" BASIS,
 - WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 - See the License for the specific language governing permissions and
 - limitations under the License.
 -}
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
