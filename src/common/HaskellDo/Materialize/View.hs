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
module HaskellDo.Materialize.View where

import Prelude hiding (div, id)

import GHCJS.HPlay.View hiding (addHeader, atr)
import AxiomUtils

container :: Perch -> Perch
container = div ! atr "class" "container"

row :: Perch -> Perch
row = div ! atr "class" "row"

col :: String -> Int -> Perch -> Perch
col size number = div ! atr "class" colClass
  where
    colClass = "col " ++ size ++ show number
