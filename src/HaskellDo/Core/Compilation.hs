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

compile :: FilePath -> String -> Cloud (Either Error Output)
compile projectPath inp = local . oneThread . liftIO $ performCompilation projectPath inp


performCompilation :: FilePath -> String -> IO (Either Error Output)
performCompilation projectPath inp = do
    writeCode (projectPath ++ "/src/Main.hs") inp
    buildHtmlCode projectPath

buildHtmlCode :: FilePath -> IO (Either Error Output)
buildHtmlCode projectPath = do
    (exitCode, _, err) <- System.readCreateProcessWithExitCode (System.shell $ "cd " ++ projectPath ++ " && stack build") ""
    case exitCode of
        System.ExitFailure _ -> Left <$> buildError err
        System.ExitSuccess   -> Right <$> buildOutput projectPath

buildError :: String -> IO Error
buildError err = do
    let prettyError = "<div class=\"card-panel red darken-1 white-text\" role=\"alert\">"
                ++ err
                ++ "</div>"
    return prettyError

buildOutput :: FilePath -> IO Output
buildOutput projectPath = do
    (exitCode, out, _) <- System.readCreateProcessWithExitCode (System.shell $ "cd " ++ projectPath ++ " && stack exec run-test") ""
    case exitCode of
        System.ExitFailure _ -> return "Compiling..."
        System.ExitSuccess   -> return $ build out
  where
    build out =
        Text.pack out
        |> Text.replace "div class=\"container\"" "div onload=\"$('.haskell').each(function(i, block){ hljs.highlightBlock(block);})\""
        |> Text.replace "<link href=\"https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css\" rel=\"stylesheet\">" ""
        |> Text.replace "<link href=\"https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap-theme.min.css\" rel=\"stylesheet\">" ""
        |> Text.replace "<script src=\"https://ajax.googleapis.com/ajax/libs/jquery/1.12.4/jquery.min.js\"></script>" ""
        |> Text.replace "<script src=\"https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js\"></script>" ""
        |> Text.unpack


writeCode :: FilePath -> String -> IO ()
writeCode path code = do
    let fileContent = "{-# OPTIONS_GHC -F -pgmF inlitpp #-}\n" ++
                      "```haskell hide top\n" ++
                      "import Inliterate.Import\n" ++
                      "```\n" ++
                      code
    writeFile path (fromString fileContent)
