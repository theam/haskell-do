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
module HaskellDo.SimpleMDE.View where

import Prelude hiding (div, id)
import Control.Monad.IO.Class

import GHCJS.HPlay.View hiding (addHeader, atr)
import AxiomUtils

import HaskellDo.SimpleMDE.Types
import Foreign.SimpleMDE

initialize :: IO ()
initialize =
    addHeader $ do
        link ! atr "rel" "stylesheet"
             ! href "https://cdn.jsdelivr.net/simplemde/latest/simplemde.min.css"
        script ("var simpleMDE;" :: String)
        script ! src "https://cdn.jsdelivr.net/simplemde/latest/simplemde.min.js"
               $ noHtml
        script initScript
  where
    initScript =
        concat  [ "function initMDE() {"
                ,    "if (typeof SimpleMDE !== 'undefined') {"
                ,        "simpleMDE=new SimpleMDE({"
                ,           "tabSize: 2, "
                ,           "status: false,"
                ,           "toolbar: false,"
                ,           "previewRender: function(x){return '';},"
                ,           "autofocus: true,"
                ,           "forceSync: true,"
                ,           "indentWithTabs: false"
                ,        "});"
                ,    "} else {"
                ,        "window.setTimeout(initMDE, 10);"
                ,    "}"
                , "};"
                , "initMDE();"
                ]

view :: State -> Widget Action
view _ = do
    _ <- textArea "" `fire` OnKeyUp
    newContent <- liftIO getMDEContent
    return (NewContent newContent)
