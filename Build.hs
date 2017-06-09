#!/usr/bin/env stack
-- stack --resolver lts-8.11 --install-ghc runghc --package turtle-1.3.2 --package foldl
{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (FilePath)
import Turtle
import Control.Monad (when)
import Data.Text as T
import Data.Text (Text)
import System.Info (os)
import qualified Control.Foldl as Foldl
import Filesystem.Path.CurrentOS
import Filesystem

clientStackYaml = "client-stack.yaml"
serverStackYaml = "stack.yaml"

main = do
  projectDirectory <- pwdAsText
  BuildCommand all gui core orchestrator run pkg <- options "Haskell.do build file" buildSwitches
  if all
    then buildAll projectDirectory
    else do
      when gui          $ buildGUI          projectDirectory
      when core         $ buildCore         projectDirectory
      when orchestrator $ buildOrchestrator projectDirectory
      when run          $ runHaskellDo      projectDirectory
      when pkg          $ buildAndPackage   projectDirectory


buildSwitches :: Parser BuildCommand
buildSwitches = BuildCommand
     <$> switch "all"          'a' "Build all subprojects, without running Haskell.do"
     <*> switch "gui"          'g' "Build GUI"
     <*> switch "core"         'c' "Build processing/compilation core"
     <*> switch "orchestrator" 'o' "Build orchestrator"
     <*> switch "run"          'r' "Run Haskell.do"
     <*> switch "package"      'p' "Package Haskell.do for release (caution: removes .stack-work before re-building)"

buildAll projectDirectory = do
  buildCore projectDirectory
  buildGUI projectDirectory
  buildOrchestrator projectDirectory

buildCore :: Text -> IO ()
buildCore pdir = do
  echo "Building core"
  exitCode <- shell ("stack build --stack-yaml=" <> serverStackYaml) ""
  when (exitCode /= ExitSuccess) (error "Core: Build failed")
  return ()


buildGUI pdir =
  if isWindows os
    then die "GHCJS currently does not support Windows, please try from a *nix machine."
    else do
      echo "Building GUI"
      shell "mkdir -p static" ""
      Just directory <- fold (inshell ("stack path --stack-yaml=" <> clientStackYaml <> " --local-install-root") Turtle.empty) Foldl.head
      Just coreBinDirectory <- fold (inshell "stack path --local-install-root" Turtle.empty) Foldl.head
      exitCode <- shell ("stack build --stack-yaml=" <> clientStackYaml) ""
      when (exitCode /= ExitSuccess) (error "GUI: Build failed")
      shell "rm -rf static/out.jsexe/*.js" ""
      shell "rm -rf static/out.jsexe/*.externs" ""
      shell "rm -rf static/out.jsexe/*.stats" ""
      shell "rm -rf static/out.jsexe/*.webapp" ""
      shell ("cp -R " <> lineToText directory <> "/bin/haskell-do.jsexe/*.js static/out.jsexe") ""
      shell ("cp -R static " <> lineToText coreBinDirectory <> "/bin") ""
      return ()

buildAndPackage projectDirectory = do
  removeTree ".stack-work"
  removeTree ".build-dist"
  buildAll projectDirectory
  
  createDirectory True ".build-dist"
  rename "static" (".build-dist" </> "static")
  (_, binPath) <- shellStrict "stack exec which haskell-do" ""
  case textToLine binPath of
    Just path -> copyFile (fromText . lineToText $ path) (".build-dist" </> "haskell-do")
    Nothing -> return ()
  shell "cd .build-dist; zip -r ../release.zip *" ""
  rename (".build-dist" </> "static") "static"

buildOrchestrator pdir =
  echo ""


runHaskellDo pdir = do
  echo "Running Haskell.do"
  shell ("stack exec haskell-do --stack-yaml=" <> serverStackYaml <> " -- 8080") ""
  return ()

-- Helpers
isWindows operatingSystem = "mingw" `T.isPrefixOf` T.pack operatingSystem
isOSX operatingSystem = "darwin" `T.isPrefixOf` T.pack operatingSystem

makeTextPath = T.pack . encodeString . fromText

pwdAsText :: IO Text
pwdAsText = T.pack <$> encodeString <$> pwd

data BuildCommand = BuildCommand
  { buildCommandAll          :: Bool
  , buildCommandGui          :: Bool
  , buildCommandCore         :: Bool
  , buildCommandOrchestrator :: Bool
  , buildCommandRun          :: Bool
  , buildCommandPackage      :: Bool
  }
