module Integration.Main (fakeWorldIntegrationTestCases) where

import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase, (@?=))

import Control.Monad.State.Strict (State, runState, get, put)
import Control.Monad.Trans.Either (EitherT, runEitherT)
import Data.List (intercalate)

import Types (Tag(..), Version(..))
import Interpreter.State (interpret, defaultWorld, World(..))
import Program.Release (program)


data FakeWorldTestCase = FakeWorldTestCase {
    testDescription :: String
  , startWorld      :: World
  , endWorld        :: World
  }

fakeWorldIntegrationTestCases :: [TestTree]
fakeWorldIntegrationTestCases = map fakeWorldTestCase testCases

testCases :: [FakeWorldTestCase]
testCases = [successful, failing]
  where
    successful = FakeWorldTestCase {
        testDescription = "successful release"
      , startWorld = defaultWorld {
            wReleaseBranchName = "apples"
          , wTags = [
              ReleaseTag $ SemVer 1 2 3
            , CiTag $ SemVer 1 1 1
            ]
          }
      , endWorld = defaultWorld {
            wReleaseBranchName = "apples"
          , wCurrentDeployment = Just $ ReleaseCandidateTag (SemVer 1 3 3) 1
          , wTags = [
                ReleaseTag $ SemVer 1 2 3
              , CiTag $ SemVer 1 1 1
              ]

          , wBranches = [("apples","ci/1.1.1")]
          , wLog = [
              "Cut release branch, apples"
            , "Deployed to preproduction"
            , "Release candidate release/1.3.3-rc1/apples has been deployed. Evaluate this release on http://preprod.gust.com."
            ]
          }

      }

    failing = FakeWorldTestCase {
        testDescription = "failing release"
      , startWorld = defaultWorld {
          wTags = [
              CiTag $ SemVer 1 1 1
            ]
          }
      , endWorld = defaultWorld {
            wCurrentDeployment = Nothing
          , wTags = [
                CiTag $ SemVer 1 1 1
              ]

          , wBranches = [("bananas","ci/1.1.1")]
          , wLog = [
              "Cut release branch, bananas"
            , "Could not find latest release tag"
            ]
          }

      }



fakeWorldTestCase :: FakeWorldTestCase -> TestTree
fakeWorldTestCase tc = testCase (testDescription tc) $
 run (startWorld tc) @?= endWorld tc
  where
    run :: World -> World
    run startWorld = endWorld
      where 
        ((), endWorld) = runState stateInterpretation startWorld
          where
            stateInterpretation :: State World ()
            stateInterpretation = do
              eitherResult <- runEitherT $ interpret program
              logMessages $ either (:[]) id eitherResult

              where
                logMessages :: [String] -> State World ()
                logMessages e = do
                  w <- get
                  put w{wLog = e ++ (wLog w)}

