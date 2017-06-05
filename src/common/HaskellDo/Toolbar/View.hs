module HaskellDo.Toolbar.View where


import Prelude hiding (div, id)

import GHCJS.HPlay.View hiding (addHeader, atr, id, wlink)
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
            li ! id "packageEditorButton" $ noHtml
    packageEditorModal    -- Apparently, if we put this line
    openProjectModal      -- under this one. The open project modal doesn't work

openProjectModal :: Perch
openProjectModal =
    div ! id "openProjectModal" ! atr "class" "modal modal-fixed-footer" $ do
        div ! atr "class" "modal-content" $ do
            h4 ("Open project" :: String)
            div $ do
                b ("Path to Stack project" :: String)
                div ! id "pathInput" $ noHtml
                p ! atr "class" "grey-text lighten-4" $ ("Path must be absolute, without ~ or environment variables." :: String)
                div ! id "creationDisplay" $ noHtml
        div ! atr "class" "modal-footer" $
            div ! id "closeModalButton" $ noHtml

packageEditorModal :: Perch
packageEditorModal =
    div ! id "packageEditorModal" ! atr "class" "modal bottom-sheet" $ do
        div ! atr "class" "modal-content" $ do
            h4 ("Project settings" :: String)
            div $
                div ! id "packageTextArea" $ noHtml
        div ! atr "class" "modal-footer" $ do
            p ! atr "class" "red-text" $ ("Dependencies will be downloaded after confirming" :: String)
            div ! id "cancelPackageEditorButton" $ noHtml
            div ! id "closePackageEditorButton" $ noHtml

openProjectButton :: State -> Widget Action
openProjectButton _ = Ulmus.newWidget "openProjectButton" $ wlink OpenProject $
        a ! atr "class" "btn-floating purple darken-2 tooltipped" ! atr "data-position" "bottom" ! atr "data-tooltip" "New/Open" ! atr "data-delay" "50" $
            i ! atr "class" "material-icons" $ ("folder_open" :: String)

packageEditorButton :: State -> Widget Action
packageEditorButton _ = Ulmus.newWidget "packageEditorButton" $ wlink LoadPackageYaml $
        a ! atr "class" "btn-floating purple darken-2 tooltipped" ! atr "data-position" "bottom" ! atr "data-tooltip" "Project settings"  ! atr "data-delay" "50"$
            i ! atr "class" "material-icons" $ ("build" :: String)


compileButton :: State -> Widget Action
compileButton _ = Ulmus.newWidget "compileButton" $ wlink Compile $
    a ! atr "class" "btn-floating purple darken-2 tooltipped" ! atr "data-position" "bottom" ! atr "data-tooltip" "Compile [Ctrl+Return]" ! atr "data-delay" "50"$
        i ! atr "class" "material-icons" $ ("play_arrow" :: String)

closeModalButton :: State -> Widget Action
closeModalButton _ = Ulmus.newWidget "closeModalButton" $ wlink LoadProject $
     a ! atr "class" "modal-action modal-close waves-effect btn-flat waves-purple" $
        i ! atr "class" "material-icons" $ ("input" :: String)

closePackageEditorButton :: State -> Widget Action
closePackageEditorButton _ = Ulmus.newWidget "closePackageEditorButton" $ wlink SavePackage $
    a ! atr "class" "modal-action modal-close waves-effect btn-flat waves-purple" $
        i ! atr "class" "material-icons" $ ("playlist_add_check" :: String)

cancelPackageEditorButton :: State -> Widget Action
cancelPackageEditorButton _ = Ulmus.newWidget "cancelPackageEditorButton" $ wlink ClosePackageModal $
    a ! atr "class" "modal-action modal-close waves-effect btn-flat waves-purple" $
        i ! atr "class" "material-icons" $ ("clear" :: String)

pathInput :: State -> Widget Action
pathInput state = Ulmus.newWidget "pathInput" $ do
    let pr = if lastProject state == ""
             then Nothing
             else Just $ lastProject state
    _ <- getString pr
            ! atr "placeholder" "/path/to/your/project"
            `fire` OnKeyUp
    projPath <- liftIO $ getValueFromId "#pathInput event input"
    return $ NewPath projPath


packageTextArea :: State -> Widget Action
packageTextArea _ = Ulmus.newWidget "packageTextArea" $ do
     _ <- getMultilineText "" ! atr "rows" "20" `fire` OnKeyUp
     newConfig <- liftIO $ getValueFromId "#packageTextArea event textarea"
     return $ NewPackage newConfig

creationDisplay :: State -> Widget ()
creationDisplay _ = Ulmus.newWidget "creationDisplay" $
    rawHtml $ p ! atr "class" "red-text" $ ("" :: String)
