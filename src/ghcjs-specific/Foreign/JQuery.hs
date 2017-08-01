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

-- go through script tags, loading each one in order
-- the deferred object and $.when make sure scripts are executed
-- sequentially
foreign import javascript unsafe "setTimeout(function() { \
                                    var scripts = []; \
                                    $($1).find('script').each(function() { \
                                      var $e = $(this); \
                                      $.when.apply($, scripts).then(function() { \
                                        if ($e.attr('src')) { \
                                          var d = $.Deferred(); \
                                          $.getScript($e.attr('src'), function() { d.resolve() }); \
                                          scripts.push(d); \
                                        } else { \
                                          eval($e.html()); \
                                        } \
                                      }) \
                                    }) \
                                  }, 0);"
  js_activateScriptTags :: JSString -> IO ()

foreign import javascript unsafe "setTimeout(function() { $($1).height($($2).height()) }, 0);"
  js_setHeightFromElement :: JSString -> JSString -> IO ()

getValueFromId :: String -> IO String
getValueFromId s = do
  r <- js_getValueFromId $ pack s
  return $ unpack r

setValueForId :: String -> String -> IO ()
setValueForId id' s = js_setValueForId (pack id') (pack s)

setHtmlForId :: String -> String -> IO ()
setHtmlForId id' s = js_setHtmlForId (pack id') (pack s)

activateScriptTags :: String -> IO ()
activateScriptTags id' = js_activateScriptTags (pack id')

setHeightFromElement :: String -> String -> IO ()
setHeightFromElement id' id'' = js_setHeightFromElement (pack id') (pack id'')

show :: String -> IO ()
show = js_show . pack

hide :: String -> IO ()
hide = js_hide . pack

shake :: String -> IO ()
shake = js_shake . pack
