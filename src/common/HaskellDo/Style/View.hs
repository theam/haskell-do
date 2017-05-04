module HaskellDo.Style.View (initialize) where

import Clay
import GHCJS.HPlay.View hiding (addHeader, height, (!))
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
    ".CodeMirror" ? do
        fontFamily [] [monospace]
        borderWidth (px 0)
    ".fixed-action-btn" ? do
        position fixed
        top (pct 2)
        right (pct 2)
