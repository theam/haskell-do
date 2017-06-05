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
module AxiomUtils where

import GHCJS.HPlay.View (setHtml, Attribute, Perch, Widget, BrowserEvent(..), a, (!), href, pass)
import qualified GHCJS.HPlay.View
import GHCJS.Types
import Data.JSString

setContents :: Perch -> String -> Perch
setContents element content = element `setHtml` (pack content :: JSString)

addHeader :: Perch -> IO ()
addHeader = GHCJS.HPlay.View.addHeader

atr :: String -> String -> Attribute
atr propName propValue = GHCJS.HPlay.View.atr propName (pack propValue :: JSString)

id :: String -> Attribute
id = atr "id"

at :: String -> GHCJS.HPlay.View.UpdateMethod -> GHCJS.HPlay.View.Widget a -> GHCJS.HPlay.View.Widget a
at s = GHCJS.HPlay.View.at (pack s :: JSString)

wlink :: a -> Perch -> Widget a
wlink x v =  do
    _ <- (a ! href "#"   $ v)  `pass` OnClick
    return x
