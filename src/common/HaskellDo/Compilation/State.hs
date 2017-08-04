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
import Control.Monad
import Control.Monad.IO.Class
import qualified System.Process as System
import qualified System.Exit as System
import qualified Data.Text as Text
import System.Directory
import System.FilePath ((</>), takeDirectory, takeFileName, dropTrailingPathSeparator)

import Transient.Move

import HaskellDo.Compilation.Types

initialState :: State
initialState = State
    { compiledOutput = ""
    , compilationError = "No project has been loaded yet, try opening one?"
    , projectPath = ""
    , workingFile = "src/Main.hs"
    , dirtyCompile = True
    }

lastProjectFile :: FilePath
lastProjectFile = "lastproject"

templateURL :: String
templateURL = "https://raw.githubusercontent.com/theam/stack-templates/master/haskell-do-new.hsfiles"

update :: Action -> State -> Cloud State
update (WriteWorkingFile content) state = localIO $ do
    unless (null $ projectPath state) (writeWorkingFile content state)
    return state
update Compile state = do
    localIO $ writeFile lastProjectFile $ projectPath state
    compile state

writeWorkingFile :: String -> State -> IO ()
writeWorkingFile content state = do
    let fullPath = projectPath state </> workingFile state
    fileExists <- doesFileExist fullPath
    when fileExists (writeCode fullPath content)

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
    putStrLn $ "Attempting to build " ++ projectPath state
    (exitCode, _, err) <- runCommand "build" (projectPath state)
    case exitCode of
        System.ExitFailure _ ->
            if isCommonError err
                then buildOutput state
                else return state { compilationError = err, dirtyCompile = True }
        System.ExitSuccess ->
            buildOutput state
  where
    isCommonError err =
        "package database" `Text.isInfixOf` Text.pack err

buildOutput :: State -> IO State
buildOutput state = do
    (exitCode, out, _) <- runCommand "exec run-test" (projectPath state)
    case exitCode of
        System.ExitFailure _ ->
            return state { compiledOutput = "Compiling" }
        System.ExitSuccess ->
            return state { compiledOutput = preprocessOutput out, compilationError = "", dirtyCompile = True }

preprocessOutput :: String -> String
preprocessOutput out =
    Text.pack out
    |> remove "class=\"container\""
    |> remove bootstrapCSSTag
    |> remove bootstrapThemeCSSTag
    |> remove jQueryJSTag
    |> remove bootstrapJSTag
    |> Text.unpack
  where
    remove s = Text.replace s ""

makeNewProject :: String -> IO ()
makeNewProject path = do
    let p = dropTrailingPathSeparator path
    let projectName = takeFileName p
    let parentDir = takeDirectory p
    putStrLn path
    putStrLn projectName
    putStrLn parentDir

    exists <- doesDirectoryExist path
    if exists then do
      isEmpty <- null <$> listDirectory path
      when isEmpty $ do
        _ <- runCommand ("new " ++ projectName ++ " " ++ templateURL ++ " --bare") path
        return ()
    else do
      _ <- runCommand ("new " ++ projectName ++ " " ++ templateURL) parentDir
      return ()

runCommand :: String -> FilePath -> IO (System.ExitCode, String, String)
runCommand command projPath = do
    putStrLn $ "Executing: cd " ++ projPath ++ " && " ++ stackCommand ++ " " ++ command
    System.readCreateProcessWithExitCode (System.shell
        $ "cd " ++ projPath ++ " && " ++ stackCommand ++ " " ++ command
        ) ""
  where
    stackCommand = "stack"


bootstrapCSSTag :: Text.Text
bootstrapCSSTag = "<link href=\"https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css\" rel=\"stylesheet\">"

bootstrapThemeCSSTag :: Text.Text
bootstrapThemeCSSTag = "<link href=\"https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap-theme.min.css\" rel=\"stylesheet\">"

bootstrapJSTag :: Text.Text
bootstrapJSTag = "<script src=\"https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js\"></script>"

jQueryJSTag :: Text.Text
jQueryJSTag = "<script src=\"https://ajax.googleapis.com/ajax/libs/jquery/1.12.4/jquery.min.js\"></script>"
