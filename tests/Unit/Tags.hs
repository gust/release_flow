module Unit.Tags (getAllCandidatesForReleaseUnitTests) where

import           Test.Tasty       (TestTree)
import           Test.Tasty.HUnit (testCase, (@?=))

import           Tags             (getAllCandidatesForRelease)
import           Types


getAllCandidatesForReleaseUnitTests :: [TestTree]
getAllCandidatesForReleaseUnitTests = map (\(description, releaseTag, tags, candidateTags) ->
  testCase description $ getAllCandidatesForRelease releaseTag tags @?= candidateTags) allTestCases
    where

      allTestCases = concat [
          simpleTestCases
        ]

      simpleTestCases = [
          (   "no candidates at all"
            , ReleaseTag $ SemVer 1 2 0
            , [
                ReleaseTag $ SemVer 1 2 0
              ]
            , []
          )
        , (   "candidate version does not match the requested release version"
            , ReleaseTag $ SemVer 1 2 0
            , [
                ReleaseCandidateTag (SemVer 1 3 0) 1
              ]
            , []
          )
        , ( "two candidates match"
            , ReleaseTag $ SemVer 1 3 0
            , [
                ReleaseCandidateTag (SemVer 1 2 0) 1
              , ReleaseCandidateTag (SemVer 1 2 0) 2
              , ReleaseTag $ SemVer 1 2 0
              , ReleaseCandidateTag (SemVer 1 3 0) 1
              , ReleaseCandidateTag (SemVer 1 3 0) 2
              ]
            , [
                ReleaseCandidateTag (SemVer 1 3 0) 1
              , ReleaseCandidateTag (SemVer 1 3 0) 2
              ]
          )
        , ( "matching candidate for a new release patch version"
            , ReleaseTag $ SemVer 1 3 1
            , [
                ReleaseCandidateTag (SemVer 1 2 0) 1
              , ReleaseCandidateTag (SemVer 1 2 0) 2
              , ReleaseTag $ SemVer 1 2 0
              , ReleaseCandidateTag (SemVer 1 3 0) 1
              , ReleaseTag $ SemVer 1 3 0
              , ReleaseCandidateTag (SemVer 1 3 1) 1
              ]
            , [
                ReleaseCandidateTag (SemVer 1 3 1) 1
              ]
          )

        ]
