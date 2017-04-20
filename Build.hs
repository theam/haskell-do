#!/usr/bin/env stack
-- stack --resolver lts-8.6 --install-ghc runghc --package turtle-1.3.2 --package foldl
{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (FilePath)
import Turtle
import Control.Monad (when)
import Data.Text as T
import Data.Text (Text)
import System.Info (os)
import qualified Control.Foldl as Foldl
import Filesystem.Path.CurrentOS 

clientStackYaml = "client-stack.yaml"
serverStackYaml = "stack.yaml"

main = do
  projectDirectory <- pwdAsText
  BuildCommand all gui core orchestrator run <- options "Haskell.do build file" buildSwitches
  if all
    then buildAll projectDirectory
    else do
      when gui          $ buildGUI          projectDirectory
      when core         $ buildCore         projectDirectory
      when orchestrator $ buildOrchestrator projectDirectory
      when run          $ runHaskellDo      projectDirectory


buildSwitches :: Parser BuildCommand
buildSwitches = BuildCommand
     <$> switch "all"          'a' "Build all subprojects, without running Haskell.do"
     <*> switch "gui"          'g' "Build GUI"
     <*> switch "core"         'c' "Build processing/compilation core"
     <*> switch "orchestrator" 'o' "Build orchestrator"
     <*> switch "run"          'r' "Run Haskell.do"

buildAll projectDirectory = do
  buildCore projectDirectory
  buildGUI projectDirectory
  buildOrchestrator projectDirectory

buildCore :: Text -> IO ()
buildCore pdir = do
  echo "Building core"
  shell ("stack build --stack-yaml=" <> serverStackYaml) ""
  return ()


buildGUI pdir = 
  if isWindows os
    then die "GHCJS currently does not support Windows, please try from a *nix machine."
    else do
      echo "Building GUI"
      shell "mkdir -p static" ""
      Just directory <- fold (inshell "stack path --stack-yaml=client-stack.yaml --local-install-root" Turtle.empty) Foldl.head
      shell ("stack build --stack-yaml=" <> clientStackYaml) ""
      shell "rm -rf static/out.jsexe" ""
      shell ("cp -R " <> lineToText directory <> "/bin/haskell-do.jsexe static/out.jsexe") ""
      return ()


buildOrchestrator pdir = 
  echo "Building orchestrator"


runHaskellDo pdir = do
  echo "Running Haskell.do"
  shell ("stack exec haskell-do --stack-yaml=" <> serverStackYaml <> " -- web") ""
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
  }

