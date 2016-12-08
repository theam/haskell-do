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

isModule (' ' : xs) = isModule xs
isModule ('m':'o':'d':'u':'l':'e':xs) = True

preprocess x = do
  a <- x
  case a of
    Left a -> pure (Left (show a))
    Right x -> pure (Right x)

notebookInterpreter :: Notebook -> IO (Either String Notebook)
notebookInterpreter code = preprocess . runInterpreter $ do
    setImportsQ[("Prelude",Nothing)]
    let newIds = [maximum (cellId <$> cells code) + 1 ..]
    newCells <- sequence $ do
      (c, newId) <- cells code `zip` newIds
      case cellType c of
        DisplayCell -> []
        TextCell -> pure $ pure c
        CodeCell -> do
          let str = cs <$> eval (cs $ cellContent c)
          [pure c, Cell DisplayCell newId <$> str]
    return $ code { cells = newCells }
