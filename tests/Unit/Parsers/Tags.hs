module Unit.Parsers.Tags (tagsParsersUnitTests) where

import           Test.Tasty                    (TestTree)
import           Test.Tasty.HUnit              (testCase, (@?=))
import           Text.ParserCombinators.Parsec (parse)

import           Parser.Tag                    (parsedTags)
import           Types

tagsParsersUnitTests :: [TestTree]
tagsParsersUnitTests = map (\(s, eitherTags, description) ->
  testCase description $
  parsedTags s @?= eitherTags) allTestCases
    where

      allTestCases = concat [
          simpleTestCases
        ]

      simpleTestCases = [
          ("release/1.23.5\n",      Right [ReleaseTag $ SemVer 1 23 5],                   "single release tag")
        , ("release/1.2.0-rc3\n",   Right [ReleaseCandidateTag (SemVer 1 2 0) 3],         "single release candidate tag")
        , ("ci/142342344\n",        Right [CiTag $ UnixTimeVer 142342344],                "single ci tag")
        , ("release/1.0.0-rc1\n" ++
            "ci/999229294\n" ++
            "release/1.3.8\n"
          ,
          Right [
              ReleaseCandidateTag (SemVer 1 0 0) 1
            , CiTag $ UnixTimeVer 999229294
            , ReleaseTag $ SemVer 1 3 8
            ],
          "multiple various tags"
          )
        , ("what?/142342344\n" ++
            "ci/142355555\n" ++
            "release/9.9\n",

          Right [CiTag $ UnixTimeVer 142355555],
          "extract only good and known type of tags")
        ]
