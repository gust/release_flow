{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.Trans.Either (runEitherT)
import Data.List (intercalate)

import Program.Release (program)
import Interpreter.IO (interpret)

main :: IO ()
main = do
  eitherResult <- runEitherT $ interpret program
  case eitherResult of
    Right messages -> do
      logError $ "Log: " ++ intercalate "\n" messages
    Left err -> do
      logError $ "Error: " ++ err

  where
    logError :: String -> IO ()
    logError = putStrLn



