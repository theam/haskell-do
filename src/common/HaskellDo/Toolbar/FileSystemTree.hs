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
module HaskellDo.Toolbar.FileSystemTree (widget) where

import Prelude hiding (div, id, span)

import GHCJS.HPlay.View hiding (addHeader, atr, id, wlink)
import AxiomUtils
import qualified Ulmus

import System.FilePath ((</>), takeDirectory, dropTrailingPathSeparator)

import HaskellDo.Toolbar.Types

widget :: State -> Widget Action
widget state = Ulmus.newWidget "fsTree" $
    if null pp then
      return $ NewPath ""
    else
      if directoryExists state
      then
        let dirElements = map directoryItem directories
            fileElements = map fileItem files

            elements = dirElements ++ fileElements
            final = tools : elements
        
        in foldl1 (<|>) final
      else
        noWidget
  where
    (directories, files) = directoryList state
    pp = projectPath state

    folderIcon = i ! atr "class" "material-icons amber-text text-darken-1" $ ("folder" :: String)
    fileIcon = i ! atr "class" "material-icons blue-grey-text text-lighten-2" $ ("insert_drive_file" :: String)
    backIcon = i ! atr "class" "material-icons amber-text text-lighten-1" $ ("arrow_back" :: String)
    newDirIcon = i ! atr "class" "material-icons amber-text text-lighten-1" $ ("create_new_folder" :: String)

    directoryItem name = item (NewPath $ pp </> name) folderIcon name
    fileItem = item (NewPath pp) fileIcon

    tools = Ulmus.newWidget "fsTree-tools" $
              backItem <|> newDirectoryItem

    backItem = tool (NewPath $ parentDirectory pp) backIcon ("Back" :: String) ! atr "class" "valign-wrapper left"
    newDirectoryItem = tool NewDirectoryModal newDirIcon ("New Directory" :: String) ! atr "class" "valign-wrapper right"

parentDirectory :: FilePath -> FilePath
parentDirectory = takeDirectory . dropTrailingPathSeparator

item :: Action -> Perch -> String -> Widget Action
item action icon name = wlink action (li ! atr "class" "valign-wrapper" $ icon >> span name) ! atr "class" "collection-item"

tool :: Action -> Perch -> String -> Widget Action
tool action icon name = wlink action (icon >> span name) ! atr "class" "valign-wrapper"

