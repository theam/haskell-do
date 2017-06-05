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
module Main where

import HaskellDo
import System.Environment (getArgs)

defaultPort :: Integer
defaultPort = 3001

main :: IO ()
main = do
    args <- getArgs
    let port = case args of
            [x] -> read x :: Integer
            _   -> defaultPort
    showWelcomeMessage
    let msg = "     Open the following URL in your browser: http://localhost:" ++ show port ++ "     "
    putStrLn $ replicate (length msg) '='
    putStrLn msg
    putStrLn $ replicate (length msg) '='
    run port

showWelcomeMessage :: IO ()
showWelcomeMessage = putStrLn $
    "\n"
    ++ " ________________\n"
    ++ "´     ,   ,      `     ,--.                    ,--.          ,--.,--.       ,--.\n"
    ++ "|    /\\| | \\     |     |  ,---.  ,--,--. ,---. |  |,-. ,---. |  ||  |     ,-|  | ,---.\n"
    ++ "|   /\\\\| | /\\    |     |  .-.  |' ,-.  |(  .-' |     /| .-. :|  ||  |    ' .-. || .-. |\n"
    ++ "| (   `| |´//)   |     |  | |  |\\ '-'  |.-'  `)|  \\  \\\\   --.|  ||  |.--.\\ `-' |' '-' '\n"
    ++ "|   \\  | |///    |     `--' `--' `--`--'`----' `--'`--'`----'`--'`--''--' `---'  `---'\n"
    ++ "|     \\| |/      |\n"
    ++ "\\________________/\n"
