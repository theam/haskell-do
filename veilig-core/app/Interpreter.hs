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
import Data.String.Conversions
import System.IO
import System.Process
import GHC.IO.Handle

isModule (' ' : xs) = isModule xs
isModule ('m':'o':'d':'u':'l':'e':xs) = True

preprocess x = do
  a <- x
  case a of
    Left a -> pure (Left (show a))
    Right x -> pure (Right x)

notebookInterpreter :: Notebook -> Handle -> Handle -> IO (Either String Notebook)
notebookInterpreter code inp out = preprocess . runInterpreter $ do
    liftIO $ hPutStrLn inp ":r"
    let toExec = tail . dropWhile (/= '>') $ console code
    liftIO $ hPutStrLn inp toExec
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
    newConsole <- liftIO $ hGetContents out
    return $ code { cells = newCells, console = newConsole }
  where
    preformat = (++) "do\n" . unlines . map (\l -> "    " ++ l) . lines
