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
import qualified System.Exit as System
import qualified Data.Text as Text

import Transient.Base
import Transient.Move

type Error = String
type Output = String

projectPath :: String
projectPath = "/home/nick/Documents/haskell-do-test"

compile :: String -> Cloud (Either Error Output)
compile inp = local . oneThread . liftIO $ performCompilation inp


performCompilation :: String -> IO (Either Error Output)
performCompilation inp = do
    writeCode (projectPath ++ "/src/Main.hs") inp
    buildHtmlCode

buildHtmlCode :: IO (Either Error Output)
buildHtmlCode = do
    (exitCode, _, err) <- System.readCreateProcessWithExitCode (System.shell $ "cd " ++ projectPath ++ " && stack build") ""
    case exitCode of
        System.ExitFailure _ -> Left <$> buildError err
        System.ExitSuccess   -> Right <$> buildOutput

buildError :: String -> IO Error
buildError err = do
    let prettyError = "<div class=\"alert alert-danger\" role=\"alert\">"
                ++ err
                ++ "</div>"
    return prettyError

buildOutput :: IO Output
buildOutput = do
    (exitCode, out, _) <- System.readCreateProcessWithExitCode (System.shell $ "cd " ++ projectPath ++ " && stack exec run-test") ""
    case exitCode of
        System.ExitFailure _ -> return "Compiling..."
        System.ExitSuccess   -> return $ build out
  where
    build out =
        Text.pack out
        |> Text.replace "div class=\"container\"" "div"
        |> Text.unpack


writeCode :: FilePath -> String -> IO ()
writeCode path code = do
    let fileContent = "{-# OPTIONS_GHC -F -pgmF inlitpp #-}\n" ++
                      "```haskell hide top\n" ++
                      "import Inliterate.Import\n" ++
                      "```\n" ++
                      code
    writeFile path (fromString fileContent)
