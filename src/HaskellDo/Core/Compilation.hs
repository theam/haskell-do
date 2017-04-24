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
module HaskellDo.Core.Compilation where

import BasicPrelude
import Flow
import qualified System.Process as System

import Transient.Move

projectPath :: String
projectPath = "/home/nick/Documents/haskell-do-test"

compile :: String -> Cloud String
compile inp = local . liftIO $ performCompilation inp


performCompilation :: String -> IO String
performCompilation inp = do
    writeCode (projectPath ++ "/src/Main.hs") inp
    buildHtmlCode

buildHtmlCode :: IO String
buildHtmlCode = do
    _ <- System.spawnCommand ("cd " ++ projectPath ++ " && stack build")
    System.readCreateProcess (System.shell $ "cd " ++ projectPath ++ " && stack exec run-test") ""


writeCode :: FilePath -> String -> IO ()
writeCode path code = do
    let fileContent = "{-# OPTIONS_GHC -F -pgmF inlitpp #-}\n" ++
                      "```haskell hide top\n" ++
                      "import Inliterate.Import\n" ++
                      "```\n" ++
                      code
    writeFile path (fromString fileContent)
