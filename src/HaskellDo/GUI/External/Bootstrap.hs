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
         ! href (fromString "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css")
    link ! atr (fromString "rel") (fromString "stylesheet")
         ! href (fromString "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap-theme.min.css")
    script ! src (fromString "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js")
           $ noHtml

initializeJQuery = addHeader $ do
    script ! src (fromString "https://ajax.googleapis.com/ajax/libs/jquery/1.12.4/jquery.min.js")
           $ noHtml

initializeTether = addHeader $ do
    script ! src (fromString "https://cdnjs.cloudflare.com/ajax/libs/tether/1.4.0/js/tether.min.js")
           $ noHtml


#else
initializeBootstrap = return ()
initializeJQuery    = return ()
initializeTether    = return ()
#endif
