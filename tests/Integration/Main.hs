{-# LANGUAGE TemplateHaskell #-}

module Integration.Main (fakeWorldIntegrationTestCases) where

import           Test.Tasty                   (TestTree, testGroup)
import           Test.Tasty.HUnit             (testCase, (@?=))

import           Control.Monad.State.Strict   (State, get, put, runState)
import           Control.Monad.Trans.Either   (EitherT, runEitherT)
import           Data.List                    (intercalate)

import           Control.Lens                 (makeLenses, (%=), (^.))
import           Interpreter.State            (Input (..), Output (..),
                                               World (..), defaultInput,
                                               defaultWorld, initialOutput,
                                               interpret)
import           Program.Release              (program, runProgram)
import           Types                        (Tag (..), Version (..))

import           Integration.TestCases.Common (FakeWorldTestCase (..),
                                               jackShit,
                                               noReleaseInProgressStartRelease,
                                               noReleaseInProgressStartHotfix,
                                               releaseInProgressBad,
                                               releaseInProgressGoodMinorRelease,
                                               releaseInProgressGoodPatchRelease,
                                               releaseInProgressBugFoundBugIsFixed,
                                               releaseInProgressBugFoundBugIsNotFixed,
                                               hotfixInProgressHotfixFixed,
                                               hotfixInProgressHotfixNotFixed)

makeLenses ''FakeWorldTestCase

makeLenses ''Input
makeLenses ''Output
makeLenses ''World

fakeWorldIntegrationTestCases = [
    [ testGroup "Blank State" [fakeWorldTestCase jackShit] ]
  , [ testGroup "Hotfix in Progress" [
        fakeWorldTestCase hotfixInProgressHotfixFixed
      , fakeWorldTestCase hotfixInProgressHotfixNotFixed
      ]
    ]
  , [ testGroup "No Release In Progress" [
        fakeWorldTestCase noReleaseInProgressStartRelease
      , fakeWorldTestCase noReleaseInProgressStartHotfix
      ]
    ]
  , [ testGroup "Release in Progress" [
        testGroup "No Bugfix in progress" [
          testGroup "Release is Good" [
            fakeWorldTestCase releaseInProgressGoodMinorRelease
          , fakeWorldTestCase releaseInProgressGoodPatchRelease
          ]
        , testGroup "Release is Bad" [
            fakeWorldTestCase releaseInProgressBad
          ]
        ]
      , testGroup "Bugfix in progress" [
          fakeWorldTestCase releaseInProgressBugFoundBugIsNotFixed
        , fakeWorldTestCase releaseInProgressBugFoundBugIsFixed
        ]
      ]
    ]
  ]

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
                stateInterpretation = runProgram interpret program >>= either (logError . show) return

                logError :: String -> State World ()
                logError msg = wOutput.oStdErr %= (\existing -> existing ++ [msg])

