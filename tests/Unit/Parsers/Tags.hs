module Unit.Parsers.Tags (tagsParsersUnitTests) where

import Test.Tasty
import Test.Tasty.HUnit
import Text.ParserCombinators.Parsec (parse)

import Types.Types
import TagParsers (tagsParser)

tagsParsersUnitTests :: [TestTree]
tagsParsersUnitTests = map (\(s, eitherTags, failString) ->
  testCase failString $
  parse tagsParser "" s @?= eitherTags) allTestCases
    where

      allTestCases = concat [
          simpleTestCases
        ]

      simpleTestCases = [
          ("release/1.2.3",       Right [ReleaseTag $ SemVer 1 2 3],                "single release tag")
        ]
