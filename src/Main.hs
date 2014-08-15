{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad.Trans.Either (runEitherT)
import           Data.Either                (either)
import           Data.List                  (intercalate)
import           System.IO                  (stderr, hPutStrLn)

import           Interpreter.IO             (interpret)
import           Program.Release            (program, runProgram)

main :: IO ()
main = runProgram interpret program >>= either (logError . show) return where
  logError = hPutStrLn stderr

