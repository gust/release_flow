module Unit.ReleaseState (releaseStateUnitTests) where

import           Test.Tasty       (TestTree)
import           Test.Tasty.HUnit (testCase, (@?=))

import           Program.Release  (determineReleaseState)
import           Types

releaseStateUnitTests :: [TestTree]
releaseStateUnitTests = map (\(description, tags, branches, releaseState) ->
  testCase description $
  determineReleaseState tags branches @?= releaseState) allTestCases
    where

      allTestCases = concat [
          simpleTestCases
        ]

      simpleTestCases = [
          (   "no release in progress"
            , [
                ReleaseCandidateTag (SemVer 1 2 3) 3
              , ReleaseTag $ SemVer 1 2 3
              , CiTag      $ UnixTimeVer 123
              ]
            , []
            , NoReleaseInProgress $ ReleaseTag $ SemVer 1 2 3
          )
        , (   "no release in progress, no prior release candidate tags"
            , [
                ReleaseTag $ SemVer 1 2 3
              , CiTag      $ UnixTimeVer 123
              ]
            , []
            , NoReleaseInProgress $ ReleaseTag $ SemVer 1 2 3
          )
        , (   "release is in progress"
            , [
                ReleaseCandidateTag (SemVer 1 3 0) 2
              , ReleaseTag $ SemVer 1 2 3
              , CiTag      $ UnixTimeVer 123
              ]
            , []
            , ReleaseInProgress (ReleaseTag (SemVer 1 2 3)) (ReleaseCandidateTag (SemVer 1 3 0) 2)
          )
        , (   "release is in progress, no prior release tags"
            , [
                ReleaseCandidateTag (SemVer 1 3 0) 2
              , CiTag      $ UnixTimeVer 123
              ]
            , []
            , ReleaseInProgress (ReleaseTag (SemVer 0 0 0)) (ReleaseCandidateTag (SemVer 1 3 0) 2)
          )
        , (   "release is in progress, bugfix in progress"
            , [
                ReleaseCandidateTag (SemVer 1 3 0) 2
              , CiTag      $ UnixTimeVer 123
              ]
            , [
                Branch "release/1.3.0-rc2/bugs/fixing-a-hole-where-the-rain-gets-in"
              ]
            , ReleaseInProgressBugfix (ReleaseCandidateTag (SemVer 1 3 0) 2) (Branch "release/1.3.0-rc2/bugs/fixing-a-hole-where-the-rain-gets-in")
          )
        ]
