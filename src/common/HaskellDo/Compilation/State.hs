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
module HaskellDo.Compilation.State where

import Flow
import Control.Monad.IO.Class
import qualified System.Process as System
import qualified System.Exit as System
import qualified Data.Text as Text

import Transient.Base
import Transient.Move

import HaskellDo.Compilation.Types

initialState :: State
initialState = State
    { compiledOutput = ""
    , compilationError = ""
    , projectPath = "/home/nick/Documents/haskell-do-test"
    , workingFile = "/src/Main.hs"
    }

update :: Action -> State -> Cloud State
update (WriteWorkingFile content) state = local . liftIO $ do
    writeWorkingFile content state
    return state
update Compile state =
    compile state


writeWorkingFile :: String -> State -> IO ()
writeWorkingFile content state = do
    let fullPath = projectPath state ++ workingFile state
    writeCode fullPath content


writeCode :: FilePath -> String -> IO ()
writeCode path code = do
    let fileContent = "{-# OPTIONS_GHC -F -pgmF inlitpp #-}\n" ++
                      "```haskell hide top\n" ++
                      "import Inliterate.Import\n" ++
                      "```\n" ++
                      code
    writeFile path fileContent


compile :: State -> Cloud State
compile state = local $ liftIO $ buildHtmlCode state


buildHtmlCode :: State -> IO State
buildHtmlCode state = do
    (exitCode, _, err) <- runCommand "build" (projectPath state)
    case exitCode of
        System.ExitFailure _ ->
            if isCommonError err
                then buildOutput state
                else return state { compilationError = err }
        System.ExitSuccess ->
            buildOutput state
  where
    isCommonError err =
        "package database" `Text.isInfixOf` Text.pack err
        -- || "already exists" `Text.isInfixOf` Text.pack err
        -- || "neither installed" `Text.isInfixOf` Text.pack err

buildOutput :: State -> IO State
buildOutput state = do
    (exitCode, out, _) <- runCommand "exec run-test" (projectPath state)
    case exitCode of
        System.ExitFailure _ ->
            return state { compiledOutput = "Compiling" }
        System.ExitSuccess ->
            return state { compiledOutput = preprocessOutput out }


preprocessOutput :: String -> String
preprocessOutput out =
    Text.pack out
    |> remove "class=\"container\"" --"div onload=\"$('.haskell').each(function(i, block){ hljs.highlightBlock(block);})\""
    |> remove bootstrapCSSTag
    |> remove bootstrapThemeCSSTag
    |> remove jQueryJSTag
    |> remove bootstrapJSTag
    |> Text.unpack
  where
    remove s = Text.replace s ""


runCommand :: String -> FilePath -> IO (System.ExitCode, String, String)
runCommand command projPath =
    System.readCreateProcessWithExitCode (System.shell
        $ "cd " ++ projPath ++ " && stack " ++ command
        ) ""


bootstrapCSSTag :: Text.Text
bootstrapCSSTag = "<link href=\"https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css\" rel=\"stylesheet\">"

bootstrapThemeCSSTag :: Text.Text
bootstrapThemeCSSTag = "<link href=\"https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap-theme.min.css\" rel=\"stylesheet\">"

bootstrapJSTag :: Text.Text
bootstrapJSTag = "<script src=\"https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js\"></script>"

jQueryJSTag :: Text.Text
jQueryJSTag = "<script src=\"https://ajax.googleapis.com/ajax/libs/jquery/1.12.4/jquery.min.js\"></script>"
