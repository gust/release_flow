module Integration.Main where

import Test.Tasty
import Test.Tasty.HUnit
import Text.ParserCombinators.Parsec (parse)

import Types.Types
import Control.Monad.State
import TestInterpreter (interpret, EWP)

integrationTests :: [TestTree]
integrationTests = [fakeWorld]

fakeWorld = testCase "testing with a fake world" $
 run startWorld @?= expectedEndWorld
  where
    startWorld = defaultWorld {
        wTags = [
            ReleaseTag $ SemVer 1 2 3
          , CiTag $ SemVer 1 1 1
          ]
      }

    endWorld = defaultWorld {
        wTags = [
            ReleaseTag $ SemVer 1 2 3
          , CiTag $ SemVer 1 1 1
          ]
      }

    run :: EitherT String State World ()
    run = endWorld
      where 
        ((), endWorld) = runState stateInterpretation startWorld
          where
            stateInterpretation :: State World ()
            stateInterpretation = do
              eitherResult <- runEitherT $ interpret program
              case eitherResult of
                Right messages -> do
                  logError $ "Log: " ++ messages
                  return ()
                Left err -> do
                  logError $ "Error: " ++ err
                  return ()

              where
                logError :: String -> State World ()
                logError e = do
                  w <- get
                  put w{wErrors = e:(wErrors w)}

