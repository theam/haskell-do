module HaskellDo.Toolbar.View where


import Prelude hiding (div, id)

import GHCJS.HPlay.View hiding (addHeader, atr, id)
import AxiomUtils
import qualified Ulmus

import Control.Monad.IO.Class

import HaskellDo.Toolbar.Types
import Foreign.JQuery

toolbar :: Widget ()
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
            h4 ("Open project" :: String)
            div $ do
                b ("Path to Stack project" :: String)
                div ! id "pathInput" $ noHtml
        div ! atr "class" "modal-footer" ! id "closeModalButton" $ noHtml


openProjectButton :: State -> Widget Action
openProjectButton _ = Ulmus.newWidget "openProjectButton" $ wlink OpenProject $
        a ! atr "class" "btn-floating purple darken-2" $
            i ! atr "class" "material-icons" $ ("folder_open" :: String)

compileButton :: State -> Widget Action
compileButton _ = Ulmus.newWidget "compileButton" $ wlink Compile $
    a ! atr "class" "btn-floating purple darken-2" $
        i ! atr "class" "material-icons" $ ("play_arrow" :: String)

closeModalButton :: State -> Widget Action
closeModalButton _ = Ulmus.newWidget "closeModalButton" $ wlink LoadProject $
    a ! atr "class" "modal-action modal-close waves-effect btn-flat waves-purple" $
        i ! atr "class" "material-icons" $ ("input" :: String)

pathInput :: State -> Widget Action
pathInput state = Ulmus.newWidget "pathInput" $ do
    let pr = if lastProject state == ""
             then Nothing
             else Just $ lastProject state
    _ <- getString pr
            ! id "pathTextBox"
            ! atr "placeholder" "/path/to/your/project"
            `fire` OnKeyUp
    projPath <- liftIO $ getValueFromId "#pathTextBox"
    return $ NewPath projPath