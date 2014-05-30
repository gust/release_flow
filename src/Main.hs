{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad.Trans.Either (runEitherT)
import           Data.Either                (either)
import           Data.List                  (intercalate)

import           Interpreter.IO             (interpret)
import           Program.Release            (program)

main :: IO ()
main = do
  -- NOTE: the IO interpreter runs in EitherT [String] IO a monad as there may be errors on the interpreter level
  eitherResult <- runEitherT $ interpret program
  putStrLn $ either
    ("Error: " ++)
    ((++) "Log: " . intercalate "\n" )
    eitherResult

