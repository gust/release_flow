{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad.Trans.Either (runEitherT)
import           Data.Either                (either)
import           Data.List                  (intercalate)
import           System.IO                  (stderr, hPutStrLn)
import qualified Data.ByteString.Lazy as BL
import Data.Aeson (decode)

import           Interpreter.IO             (interpret)
import           Program.Release            (program, runProgram)
import Interpreter.Commands (Config(..))

{- data Config = Config { -}
    {- integrationBranch :: String -}
  {- , deployCommand     :: String -}
{- } -}





main :: IO ()
main = do
  jsonConfig <- BL.readFile "config.json" 
  case decode jsonConfig of
    Nothing -> do
      putStrLn "could not parse config.json"
    Just config -> do
      runProgram interpret (program config) >>= either (logError . show) return where
      logError = hPutStrLn stderr

