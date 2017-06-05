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
module Foreign.JQuery where

getValueFromId :: String -> IO String
getValueFromId _ = return ""

setValueForId :: String -> String -> IO ()
setValueForId _ _ = return ()

setHtmlForId :: String -> String -> IO ()
setHtmlForId _ _ = return ()

show :: String -> IO ()
show _ = return ()

hide :: String -> IO ()
hide _ = return ()

shake :: String -> IO ()
shake _ = return ()
