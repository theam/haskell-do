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
module External.Materialize where

import BasicPrelude hiding (div, id)
import GHCJS.HPlay.View


initializeMaterialize :: IO ()
initializeMaterialize = addHeader $ do
    link ! atr (fromString "rel") (fromString "stylesheet")
         ! href (fromString "https://cdnjs.cloudflare.com/ajax/libs/materialize/0.98.2/css/materialize.min.css")
    script ! src (fromString "https://cdnjs.cloudflare.com/ajax/libs/materialize/0.98.2/js/materialize.min.js")
           $ noHtml

initializeJQuery :: IO ()
initializeJQuery = addHeader $
    script ! src (fromString "https://ajax.googleapis.com/ajax/libs/jquery/1.12.4/jquery.min.js")
           $ noHtml

container :: Perch -> Perch
container childs =
    div ! atr "class" (fromString "my-container")
        $ childs

row :: Perch -> Perch
row childs =
    div ! atr "class" (fromString "row")
        $ childs

col :: String -> Int -> Perch -> Perch
col size number childs =
    div ! atr "class" (fromString $ "col " ++ size ++ show number)
        $ childs
