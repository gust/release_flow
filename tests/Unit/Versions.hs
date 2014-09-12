module Unit.Versions (versionUnitTests) where

import           Test.Tasty       (TestTree)
import           Test.Tasty.HUnit (testCase, (@?=))

import           Types            (Version(..), isNextPatchOf)


versionUnitTests :: [TestTree]
versionUnitTests = map (\(description, version1, version2, compareResult) ->
  testCase description $ (version2 `isNextPatchOf` version1) @?= compareResult) allTestCases
    where

      allTestCases = [
          (   
            "yes"
          , SemVer 1 2 0
          , SemVer 1 2 1
          , True
          )
        , (   
            "no"
          , SemVer 1 2 1
          , SemVer 1 2 1
          , False
          )
        ]

