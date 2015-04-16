{-# LANGUAGE ScopedTypeVariables #-}

import           Test.Tasty

import           Integration.Main
import           Unit.Parsers.Tags (tagsParsersUnitTests)
import           Unit.ReleaseState (releaseStateUnitTests)
import           Unit.Tags         (getAllCandidatesForReleaseUnitTests)
import           Unit.Versions     (versionUnitTests)

import Control.Applicative ((<$>))
import Data.Yaml (decodeEither)
import qualified Data.ByteString as BS

main = do
  let specFile = "spec.yaml"
  (eitherIntegrationTestCases :: Either String TopLevelSpec) <- decodeEither <$> BS.readFile specFile
  case eitherIntegrationTestCases of
    Right (TopLevelSpec config testCases) -> defaultMain $ testGroup "Tests" [allTests config testCases]
    Left err -> error $ "Could not parse " ++ specFile ++ ": " ++ (show err)

  where
    allTests config testCases = testGroup "Tasty Tests" [unitTests, integrationTests config testCases]
      where
        unitTests = testGroup "HUnit Tests" $ concat [tagsParsersUnitTests, releaseStateUnitTests, getAllCandidatesForReleaseUnitTests, versionUnitTests]
        integrationTests config testCases = testGroup "Integration Tests" $ [integrationTestTree config testCases]

