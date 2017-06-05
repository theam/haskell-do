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

import GHCJS.HPlay.View hiding (id, atr)

setContents :: Perch -> String -> Perch
setContents element _ = element

addHeader :: Perch -> IO ()
addHeader _ = return ()

atr :: String -> String -> (String, String)
atr = (,)

id :: String -> (String, String)
id = atr "id"

at :: String -> UpdateMethod -> Widget a -> Widget a
at _ _ w = w

wlink :: (Show a) => a -> Perch -> Widget a
wlink _ _= empty
