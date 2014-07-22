module Integration.TestCases.Common
  (
    noReleaseInProgress
  , releaseInProgressGood
  , releaseInProgressBad
  , FakeWorldTestCase
  )
  where


import           Interpreter.State (Input (..), Output (..), World (..),
                                    defaultInput, defaultWorld, initialOutput,
                                    interpret)
import           Types             (Tag (..), Version (..))


data FakeWorldTestCase = FakeWorldTestCase {
    _testDescription :: String
  , _input           :: Input
  , _expectedOutput  :: Output
  }

noReleaseInProgress = FakeWorldTestCase {
    _testDescription = "No release in progress, start a new release"

  , _input = defaultInput {
      _iTags = [
        ReleaseCandidateTag (SemVer 1 2 3) 3
      , ReleaseTag $ SemVer 1 2 3
      , CiTag      $ UnixTimeVer 123
      ]
    }

  , _expectedOutput = initialOutput {
      _oCommands = [
          "git tags"
        , "git tags"
        , "git checkout ci/123"
        , "git tag release/1.3.0-rc1"
        , "git push origin --tags"
        ]
    , _oLog = [
          "No outstanding release candidates found, starting new release candidate: release/1.3.0-rc1"
        {- , "Created tag: release/1.3.0-rc1" -}
        ]
  }
}

releaseInProgressGood = FakeWorldTestCase {
    _testDescription = "Release in progress, candidate is good"

  , _input = defaultInput {
      _iTags = [
        ReleaseCandidateTag (SemVer 1 3 0) 2
      , ReleaseTag $ SemVer 1 2 3
      , CiTag      $ UnixTimeVer 123
      ]
    , _iUserInput = [
        ("Is this release candidate good? y(es)/n(o): ", "y")
      ]
    }

  , _expectedOutput = initialOutput {
      _oCommands = [
          "git tags"
        , "git checkout release/1.3.0-rc2"
        , "git tag release/1.3.0"
        , "git push origin --tags"
        ]
    , _oLog = [
          "Release candidate found: release/1.3.0-rc2"
        , "Created tag: release/1.3.0, deploy to production cowboy!"
        ]
  }
}

releaseInProgressBad = FakeWorldTestCase {
    _testDescription = "Release in progress, candidate is bad"

  , _input = defaultInput {
      _iTags = [
        ReleaseCandidateTag (SemVer 1 3 0) 1
      , ReleaseCandidateTag (SemVer 1 3 0) 2
      , ReleaseTag $ SemVer 1 2 3
      , CiTag      $ UnixTimeVer 123
      ]
    , _iUserInput = [
        ("Is this release candidate good? y(es)/n(o): ", "n")
      , ("What bug are you fixing? (specify dash separated descriptor, e.g. 'theres-a-bug-in-the-code'): ", "theres-a-bug-in-the-code")
      ]
    }

  , _expectedOutput = initialOutput {
      _oCommands = [
          "git tags"
        , "git tags"
        , "git tag -d release/1.3.0-rc1"
        , "git push origin :refs/tags/release/1.3.0-rc1"
        , "git tag -d release/1.3.0-rc2"
        , "git push origin :refs/tags/release/1.3.0-rc2"
        ]
    , _oLog = [
          "Release candidate found: release/1.3.0-rc2"
        , "Removing stale release candidate tag: release/1.3.0-rc1"
        , "Removing stale release candidate tag: release/1.3.0-rc2"
        ]
  }
}

