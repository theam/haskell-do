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
module Foreign.SimpleMDE where

import GHCJS.Types
import Data.JSString

getMDEContent :: IO String
getMDEContent = unpack <$> js_getMDEContent

foreign import javascript unsafe "simpleMDE.value()"
    js_getMDEContent :: IO JSString

setMDEContent :: String -> IO ()
setMDEContent = js_setMDEContent . pack

foreign import javascript unsafe "simpleMDE.value($1)"
    js_setMDEContent :: JSString -> IO ()

makeSimpleMDEFromId :: String -> IO ()
makeSimpleMDEFromId = js_makeSimpleMDEFromId . pack

foreign import javascript unsafe
    "function initMDE() {\
      var ta = document.getElementById($1);\
      if (typeof SimpleMDE !== 'undefined') {\
        if (ta !== null) {\
            simpleMDE=new SimpleMDE({\
              tabSize: 2, \
              element: ta, \
              status: false,\
              toolbar: false,\
              previewRender: function(x){return '';},\
              autofocus: true,\
              forceSync: true,\
              indentWithTabs: false\
            });\
        } else {\
          window.setTimeout(initMDE, 10);\
        }\
      } else {\
          window.setTimeout(initMDE, 10);\
      }\
    };\
    initMDE();"

    js_makeSimpleMDEFromId :: JSString -> IO ()
