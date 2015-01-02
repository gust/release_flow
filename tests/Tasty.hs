{-# LANGUAGE ScopedTypeVariables #-}

import           Test.Tasty

import           Integration.Main
import           Unit.Parsers.Tags (tagsParsersUnitTests)
import           Unit.ReleaseState (releaseStateUnitTests)
import           Unit.Tags         (getAllCandidatesForReleaseUnitTests)

import Control.Applicative ((<$>))
import Data.Yaml (decode)
import qualified Data.ByteString as BS

main = do
  let specFile = "spec.yaml"
  (maybeIntegrationTestCases :: Maybe TestCase) <- decode <$> BS.readFile specFile
  case maybeIntegrationTestCases of
    Just testCases -> defaultMain $ testGroup "Tests" [allTests testCases]
    Nothing -> error $ "Could not parse " ++ specFile
  
  where
    allTests testCases = testGroup "Tasty Tests" [unitTests, integrationTests testCases]
      where
        unitTests = testGroup "HUnit Tests" $ concat [tagsParsersUnitTests, releaseStateUnitTests, getAllCandidatesForReleaseUnitTests]
        integrationTests testCases = testGroup "Integration Tests" $ [integrationTestTree testCases]



