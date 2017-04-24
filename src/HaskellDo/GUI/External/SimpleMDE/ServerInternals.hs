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
module HaskellDo.GUI.External.SimpleMDE.ServerInternals
  ( initializeSimpleMDE
  , simpleMDE
  , setRendered
  )
where

import BasicPrelude

import GHCJS.HPlay.View hiding (map, option,input)

newtype SimpleMDE = SimpleMDE () deriving (Read, Show)

simpleMDE :: Widget String
simpleMDE = throwBrowserError "simpleMDE"

initializeSimpleMDE :: IO ()
initializeSimpleMDE = return ()

setRendered :: String -> IO ()
setRendered _ = return ()

throwBrowserError :: String -> a
throwBrowserError fName = error $ fName ++ ": This function is supposed to run on the browser"
