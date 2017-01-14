{-# LANGUAGE OverloadedStrings #-}
module Interpreter
  ( notebookInterpreter
  ) where

import Types
import Control.Monad.Trans
import Language.Haskell.Interpreter
import Control.Monad
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.String.Conversions
import System.IO
import System.Process
import GHC.IO.Handle

preprocess x = do
  a <- x
  case a of
    Left a -> pure (Left (show a))
    Right x -> pure (Right x)

getCellText :: Cell -> Text
getCellText c = case cellType c of
  TextCell    -> T.unlines $ map (mappend "-- ") (T.lines $ cellContent c)
  CodeCell    -> cellContent c
  DisplayCell -> mempty

formatNotebook :: Notebook -> Text
formatNotebook note = T.unlines $ map getCellText (cells note)

notebookInterpreter :: Notebook -> State -> IO (Either String Notebook)
notebookInterpreter code state = preprocess . runInterpreter $ do
    liftIO . T.putStrLn $ formatNotebook code
    liftIO $ hPutStrLn (ghciInput state) ":r"
    let toExec = tail . dropWhile (/= '>') $ console code
    liftIO $ hPutStrLn (ghciInput state) toExec
    setImportsQ[ ("Prelude",Nothing)
               , ("Graphics.Rendering.Chart.Easy", Nothing)
               , ("System.IO.Unsafe", Nothing)
               , ("Graphics.Rendering.Chart.Backend.Diagrams", Nothing)
               ]
    let newIds = [maximum (cellId <$> cells code) + 1 ..]
    newCells <- sequence $ do
      (c, newId) <- cells code `zip` newIds
      case cellType c of
        DisplayCell -> []
        TextCell -> pure $ pure c
        CodeCell -> do
          let str = cs <$> eval (cs $ preformat $ cs $ cellContent c)
          [pure c, Cell DisplayCell newId <$> str]
    newConsole <- liftIO $ hGetContents (ghciOutput state)
    return $ code { cells = newCells, console = newConsole }
  where
    preformat = (++) "do\n" . unlines . map (\l -> "    " ++ l) . lines
