module HaskellDo.Toolbar.View where


import Prelude hiding (div, id)

import GHCJS.HPlay.View hiding (addHeader, atr, id)
import AxiomUtils
import qualified Ulmus

import Control.Monad
import Control.Monad.IO.Class

import qualified HaskellDo.Materialize.View as Materialize
import HaskellDo.Toolbar.Types
import Foreign.Materialize

toolbar = rawHtml $ do
    div ! atr "class" "fixed-action-btn horizontal" $ do
        a ! atr "class" "btn-floating btn-large purple" $
            i ! atr "class" "material-icons" $
                ("menu" :: String)
        ul $ do
            li ! id "openProjectButton" $ noHtml
            li ! id "compileButton" $ noHtml
    div ! id "openProjectModal" ! atr "class" "modal" $ do
        div ! atr "class" "modal-content" $ do
            h4 $ ("Open project" :: String)
            div $ do
                b $ ("Path to Stack project" :: String)
                div ! id "pathInput" $ noHtml
        div ! atr "class" "modal-footer" $ do
            a ! atr "href" "#!" ! atr "class" "modal-action modal-close waves-effect btn-flat waves-purple" $
                ("Open project" :: String)


openProjectButton :: State -> Widget Action
openProjectButton state = Ulmus.newWidget "openProjectButton" $ wlink OpenProject $
        a ! atr "class" "btn-floating purple darken-2" $
            i ! atr "class" "material-icons" $ ("folder_open" :: String)

compileButton :: State -> Widget Action
compileButton _ = Ulmus.newWidget "compileButton" $ wlink Compile $
    a ! atr "class" "btn-floating purple darken-2" $
        i ! atr "class" "material-icons" $ ("play_arrow" :: String)

pathInput :: State -> Widget Action
pathInput _ = Ulmus.newWidget "pathInput" $ do
    projectPath <- inputString Nothing `fire` OnChange
    return $ NewPath projectPath
