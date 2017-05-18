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
        boxShadow (px 0) (px 19) (px 38) (rgba 0 0 0 0.25)
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
