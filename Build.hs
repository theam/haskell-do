#!/usr/bin/env stack
-- stack --install-ghc runghc --package turtle --package foldl
{-# LANGUAGE OverloadedStrings #-}
import Prelude hiding (FilePath)
import Turtle
import Control.Monad (when)
import Data.Text as T
import System.Info (os)
import qualified Control.Foldl as Foldl
import Filesystem.Path.CurrentOS


main = do
  projectDirectory <- encode <$> pwd
  BuildCommand all gui core deps run <- options "Haskell.do build file" buildSwitches
  if all
    then buildAll projectDirectory
    else do
      when gui          $ buildGUI          projectDirectory
      when core         $ buildCore         projectDirectory
      when deps         $ buildDeps         projectDirectory
      when run          $ runHaskellDo      projectDirectory


buildSwitches :: Parser BuildCommand
buildSwitches = BuildCommand
     <$> switch "all"          'a' "Build all subprojects, without running Haskell.do"
     <*> switch "gui"          'g' "Build GUI"
     <*> switch "core"         'c' "Build processing/compilation core"
     <*> switch "deps"         'd' "Download dependencies"
     <*> switch "run"          'r' "Run Haskell.do"

buildAll projectDirectory = do
  buildCore projectDirectory
  buildGUI projectDirectory
  buildDeps projectDirectory


buildCore pdir = do
  echo "Build core"
  let coreExtension = case os of 
                          _  | isWindows os -> ".exe"
                             | otherwise    -> ""
  let coreFile = encode $ fromText $ "/bin/haskelldo-core"<>coreExtension
  let guiBinariesDir = encode $ fromText $ "/gui/dist/bin/haskelldo-core"<>coreExtension
  shell ("cd "<>pdir<>" &&\
    \cd core&&\
    \stack build") "" -- &&\
  Just bd <- Turtle.fold (inshell ("cd "<>pdir<>"&&cd core&&stack path --local-install-root") "") Foldl.head
  shell ("cp "<>bd<>coreFile<>" "<>pdir<>guiBinariesDir<>"&&cd ..") ""
  return ()


buildGUI pdir = do
  echo "Building GUI"
  shell ("cd "<>pdir<>" &&\
    \cd gui&&\
    \npm run build&&\
    \cd ..") ""
  return ()


buildDeps pdir = do
  echo "Downloading dependencies"
  shell ("cd "<>pdir<>" &&\
    \cd gui&&\
    \npm install && bower install && cd "<>pdir<>"&&\
    \cd core && stack setup &&\
    \cd ..") ""
  return ()


runHaskellDo pdir = do
  echo "Running Haskell.do"
  shell ("cd "<>pdir<>" &&\
    \cd gui&&\
    \npm run start") ""
  return ()




-- Helpers
isWindows operatingSystem = "mingw" `T.isPrefixOf` T.pack operatingSystem
isOSX operatingSystem = "darwin" `T.isPrefixOf` T.pack operatingSystem
data BuildCommand = BuildCommand
  { buildCommandAll          :: Bool
  , buildCommandGui          :: Bool
  , buildCommandCore         :: Bool
  , buildCommandOrchestrator :: Bool
  , buildCommandRun          :: Bool
  }
