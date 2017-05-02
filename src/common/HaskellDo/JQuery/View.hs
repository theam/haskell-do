module HaskellDo.JQuery.View where

import GHCJS.HPlay.View hiding (addHeader, atr)
import AxiomUtils

initialize :: IO ()
initialize = addHeader $
    script ! src "https://ajax.googleapis.com/ajax/libs/jquery/1.12.4/jquery.min.js"
           $ noHtml
