{-# LANGUAGE TemplateHaskell #-}

module Integration.Main (fakeWorldIntegrationTestCases) where

import           Test.Tasty                 (TestTree)
import           Test.Tasty.HUnit           (testCase, (@?=))

import           Control.Monad.State.Strict (State, get, put, runState)
import           Control.Monad.Trans.Either (EitherT, runEitherT)
import           Data.List                  (intercalate)

import           Interpreter.State          (World (..), defaultWorld, Input(..), defaultInput, Output(..), initialOutput, interpret)
import           Program.Release            (program)
import           Types                      (Tag (..), Version (..))
import Control.Lens ((%=), (^.), makeLenses)


data FakeWorldTestCase = FakeWorldTestCase {
    _testDescription :: String
  , _input           :: Input
  , _expectedOutput  :: Output
  }

makeLenses ''FakeWorldTestCase

makeLenses ''Input
makeLenses ''Output
makeLenses ''World

testCases :: [FakeWorldTestCase]
testCases = [successful, noReleaseTag]
  where
    successful = FakeWorldTestCase {
        _testDescription = "successful release"

      , _input = defaultInput {
          _iReleaseBranchName = "apples"
        , _iTags = [
            ReleaseTag $ SemVer 1 2 3
          , CiTag      $ UnixTimeVer 123
          ]
        }

      , _expectedOutput = initialOutput {
          _oCommands = [
              "checkout branch apples from tag ci/123"
            , "deploy release/1.3.0-rc1"
            ]
        , _oLog = [
              "Cut release branch, apples"
            , "Deployed to preproduction"
            , "Release candidate release/1.3.0-rc1 on release branch apples has been deployed. Evaluate this release on http://preprod.gust.com."
            ]
      }
    }

    noReleaseTag = FakeWorldTestCase {
        _testDescription = "No release tag present"

      , _input = defaultInput {
          _iReleaseBranchName = "apples"
        , _iTags = [
            CiTag      $ UnixTimeVer 123
          ]
        }

      , _expectedOutput = initialOutput {
          _oCommands = [
            "checkout branch apples from tag ci/123"
          ]
        , _oLog = [
            "Cut release branch, apples"
          , "Could not find latest release tag"
          ]
      }
    }


fakeWorldIntegrationTestCases :: [TestTree]
fakeWorldIntegrationTestCases = map fakeWorldTestCase testCases

fakeWorldTestCase :: FakeWorldTestCase -> TestTree
fakeWorldTestCase tc = testCase (tc^.testDescription) $
  (run (tc^.input)) @?= (tc^.expectedOutput)
  where
    run :: Input -> Output
    run input = output
      where
        output = _wOutput endWorld
          where
            ((), endWorld) = runState stateInterpretation startWorld
              where
                startWorld = defaultWorld {
                  _wInput = input
                }

                stateInterpretation :: State World ()
                stateInterpretation = do
                  eitherResult <- runEitherT $ interpret program
                  logMessages $ either (:[]) id eitherResult

                logMessages :: [String] -> State World ()
                logMessages e = wOutput.oLog %= (\message -> e ++ message)

