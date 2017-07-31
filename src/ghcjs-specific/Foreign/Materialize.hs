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
module Foreign.Materialize where

import GHCJS.Types
import Data.JSString

openModal :: String -> IO ()
openModal s = do
    makeModals
    js_openModal . pack $ s

closeModal :: String -> IO ()
closeModal = js_closeModal . pack

foreign import javascript unsafe "$($1).modal('open')"
    js_openModal :: JSString -> IO ()

foreign import javascript unsafe "$($1).modal('close')"
    js_closeModal :: JSString -> IO ()

foreign import javascript unsafe "$('.modal').modal({dismissible: true, opacity: 0.7});"
    makeModals :: IO ()

foreign import javascript unsafe "$('.tooltipped').tooltip({delay: 50});"
    initTooltips :: IO ()

foreign import javascript unsafe "$('#editor').is('visible')"
    isEditorVisible :: IO Bool

foreign import javascript unsafe "toggleEditor()"
    toggleEditor :: IO ()

foreign import javascript unsafe "$('#errorDisplay').toggle()"
    toggleError :: IO ()
