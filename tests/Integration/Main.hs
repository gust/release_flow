module Integration.Main (fakeWorldIntegrationTests) where

import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase, (@?=))

import Control.Monad.State (State, runState, get, put)
import Control.Monad.Trans.Either (EitherT, runEitherT)
import Data.List (intercalate)

import Types
import Interpreter.State (interpret, defaultWorld, World(..))
import Program.Release (program)

fakeWorldIntegrationTests :: [TestTree]
fakeWorldIntegrationTests = [fakeWorld]

fakeWorld = testCase "testing with a fake world" $
 run startWorld @?= expectedEndWorld
  where
    startWorld = defaultWorld {
        wTags = [
            ReleaseTag $ SemVer 1 2 3
          , CiTag $ SemVer 1 1 1
          ]
      }

    expectedEndWorld = defaultWorld {
        wCurrentDeployment = Just $ ReleaseCandidateTag (SemVer 1 3 3) 1
      , wTags = [
            ReleaseTag $ SemVer 1 2 3
          , CiTag $ SemVer 1 1 1
          ]

      , wBranches = [("bananas","ci/1.1.1")]
      , wLog = [
          "Cut release branch, bananas"
        , "Deployed to preproduction"
        , "Release candidate release/1.3.3-rc1/bananas has been deployed. Evaluate this release on http://preprod.gust.com."
        ]
      }

    run :: World -> World
    run startWorld = endWorld
      where 
        ((), endWorld) = runState stateInterpretation startWorld
          where
            stateInterpretation :: State World ()
            stateInterpretation = do
              eitherResult <- runEitherT $ interpret program
              case eitherResult of
                Right messages -> do
                  logMessages messages
                Left err -> do
                  logMessages [err]

              where
                logMessages :: [String] -> State World ()
                logMessages e = do
                  w <- get
                  put w{wLog = e ++ (wLog w)}

