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
module HaskellDo.Style.View (initialize) where

import Clay
import GHCJS.HPlay.View hiding (addHeader, height, (!), width)
import qualified Data.Text.Lazy as Text

import AxiomUtils

initialize :: IO ()
initialize = addHeader $
    nelem "style" `child` mdeStyle

mdeStyle :: String
mdeStyle = Text.unpack . Clay.render $ do
    "html, body" ? do
        backgroundColor "#ccc"
        height (pct 100.0)
    ".editor-container" ? do
        backgroundColor "#fff"
        marginTop (pct 5)
        marginLeft (pct 2)
        marginRight (pct 2)
        minHeight (pct 100.0)
        bottom (px 0)
        paddingTop (px 75)
        paddingLeft (px 60)
        paddingRight (px 60)
        position relative
        zIndex 1
    ".CodeMirror" ? do
        fontFamily [] [monospace]
        borderWidth (px 0)
    ".fixed-action-btn" ? do
        position fixed
        top (pct 2)
        right (pct 2)
    ".dimmedBackground" ? do
        height (pct 100)
        width (pct 100)
        position absolute
        top (pct 0)
        left (pct 0)
        zIndex 999
        backgroundColor "#FFF"
        opacity 0.8
    ".loader-align" ? do
        position absolute
        left (pct 50)
        top (pct 50)
    ".loader-align-inner" ? do
        position relative
        left (pct $ -50)
        top (pct $ -50)
    "#outputdiv" ? do
        position relative
        zIndex 1
    "#packageTextArea event textarea" ? do
        fontFamily [] [monospace]
        boxSizing borderBox
        overflow auto
        height (pct 100)
    "#dependencyMessage" ? do
        margin (px 0) auto (px 0) auto
    "#fsTree .collection-item span" ? do
        marginLeft (px 5)
    "#errorDisplay" ? do
        position fixed
        left (pct 1)
        bottom (pct 0)
        maxWidth (pct 98)
        zIndex 2
