module Unit.Parsers.Tags (tagsParsersUnitTests) where

import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase, (@?=))
import Text.ParserCombinators.Parsec (parse)

import Types
import Parser.Tag (parsedTags)

tagsParsersUnitTests :: [TestTree]
tagsParsersUnitTests = map (\(s, eitherTags, failString) ->
  testCase failString $
  parsedTags s @?= eitherTags) allTestCases
    where

      allTestCases = concat [
          simpleTestCases
        ]

      simpleTestCases = [
          ("release/1.2.3\n",       Right [ReleaseTag $ SemVer 1 2 3],                "single release tag")
        ]
