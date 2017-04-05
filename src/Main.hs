{-
 - src\Main.hs
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
{-# Language NoImplicitPrelude #-}
{-# Language DeriveGeneric #-}
{-# Language OverloadedStrings #-}
module Main where

import HaskellDo

import BasicPrelude
import Flow

import Options.Generic

-- | Command line options for Haskell.do, allowing to:
--
--    * Run as a standalone node, so another Haskell.do instance can connect to it
--
--    * Connect to a Haskell.do node, so one can use a remote server for
--    executing code
--
--    * Make a local session and run everything in the current machine.
data HaskellDoConfig
  = RunAsNode { listenOnAddress :: Maybe Text, listenOnPort :: Maybe Int }
  | ConnectTo { remoteAddress :: Text, remotePort :: Int }
  | LocalSession
  deriving (Generic, Show)

instance ParseRecord HaskellDoConfig


main :: IO ()
main = do
  config <- getRecord "Haskell.do"
  runHaskellDo config
  print (config :: HaskellDoConfig)
 where
  runHaskellDo (RunAsNode address port) = runAsNode (fromMaybe "localhost" address) (fromMaybe 3000 port)
  runHaskellDo (ConnectTo address port) = run address port
  runHaskellDo LocalSession             = run "localhost" 3000

