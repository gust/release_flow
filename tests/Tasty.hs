{- import Test.Tasty.SmallCheck as SC -}
{- import Test.Tasty.QuickCheck as QC -}
import           Test.Tasty        (TestTree, defaultMain, testGroup)

import           Integration.Main  (fakeWorldIntegrationTestCases)
import           Unit.Parsers.Tags (tagsParsersUnitTests)
import           Unit.ReleaseState (releaseStateUnitTests)
import           Unit.Tags         (getAllCandidatesForReleaseUnitTests)
import           Unit.Versions     (versionUnitTests)

main = defaultMain tests
  where
    tests :: TestTree
    {- tests = testGroup "Tests" [properties, unitTests] -}
    tests = testGroup "Tests" [allTests]

    {- properties :: TestTree -}
    {- properties = testGroup "Properties" [scProps, qcProps] -}

    {- scProps = testGroup "(checked by SmallCheck)" -}
      {- [ SC.testProperty "sort == sort . reverse" $ -}
          {- \list -> sort (list :: [Int]) == sort (reverse list) -}
      {- , SC.testProperty "Fermat's little theorem" $ -}
          {- \x -> ((x :: Integer)^7 - x) `mod` 7 == 0 -}
      {- -- the following property does not hold -}
      {- , SC.testProperty "Fermat's last theorem" $ -}
          {- \x y z n -> -}
            {- (n :: Integer) >= 3 SC.==> x^n + y^n /= (z^n :: Integer) -}
      {- ] -}

    {- qcProps = testGroup "(checked by QuickCheck)" -}
      {- [ QC.testProperty "sort == sort . reverse" $ -}
          {- \list -> sort (list :: [Int]) == sort (reverse list) -}
      {- , QC.testProperty "Fermat's little theorem" $ -}
          {- \x -> ((x :: Integer)^7 - x) `mod` 7 == 0 -}
      {- -- the following property does not hold -}
      {- , QC.testProperty "Fermat's last theorem" $ -}
          {- \x y z n -> -}
            {- (n :: Integer) >= 3 QC.==> x^n + y^n /= (z^n :: Integer) -}
      {- ] -}

      where
        allTests = testGroup "Tasty Tests" [unitTests, integrationTests]
          where
            unitTests = testGroup "HUnit Tests" $ concat [tagsParsersUnitTests, releaseStateUnitTests, getAllCandidatesForReleaseUnitTests, versionUnitTests]
            integrationTests = testGroup "Integration Tests" $ concat fakeWorldIntegrationTestCases

