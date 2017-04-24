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
module HaskellDo.GUI.External.Bootstrap where

import BasicPrelude
import GHCJS.HPlay.View

initializeBootstrap :: IO ()
initializeJQuery :: IO ()
initializeTether :: IO ()

#ifdef ghcjs_HOST_OS
initializeBootstrap = addHeader $ do
    link ! atr (fromString "rel") (fromString "stylesheet")
         ! href (fromString "https://cdnjs.cloudflare.com/ajax/libs/twitter-bootstrap/4.0.0-alpha.6/css/bootstrap.min.css")
    script ! src (fromString "https://cdnjs.cloudflare.com/ajax/libs/twitter-bootstrap/4.0.0-alpha.6/js/bootstrap.min.js")
           $ noHtml

initializeJQuery = addHeader $ do
    script ! src (fromString "https://cdnjs.cloudflare.com/ajax/libs/jquery/3.2.1/jquery.min.js")
           $ noHtml

initializeTether = addHeader $ do
    script ! src (fromString "https://cdnjs.cloudflare.com/ajax/libs/tether/1.4.0/js/tether.min.js")
           $ noHtml


#else
initializeBootstrap = return ()
initializeJQuery    = return ()
initializeTether    = return ()
#endif
