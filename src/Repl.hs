module Repl
  ( repl
  , runInputT
  , defaultSettings
  ) where

import Parser

import Control.Monad.IO.Class
import System.Console.Haskeline
import Text.Megaparsec

repl :: InputT IO ()
repl = do
  inputMaybe <- getInputLine "> "
  case inputMaybe of
    Nothing -> return () -- Ctrl + D
    Just input -> do
      liftIO $ case parse expressionParser "" input of
        Left errMsg -> print errMsg
        Right expr -> print expr
      repl
