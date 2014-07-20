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
        {- , "Created temporary bug fix branch release/1.3.0-rc1/bugs/theres-a-bug-in-the-code" -}
        {- , "Fix your bug dummy!" -}
        ]
  }
}


successful = FakeWorldTestCase {
    _testDescription = "successful release"

  , _input = defaultInput {
      _iTags = [
        ReleaseTag $ SemVer 1 2 3
      , CiTag      $ UnixTimeVer 123
      ]
    }

  , _expectedOutput = initialOutput {
      _oCommands = [
          "checkout branch apples from tag ci/123"
        , "deploy release/1.3.0-rc1"
        ]
    , _oLog = [
          "Cut release branch, apples"
        , "Deployed to preproduction"
        , "Release candidate release/1.3.0-rc1 on release branch apples has been deployed. Evaluate this release on http://preprod.gust.com."
        ]
  }
}

noReleaseTag = FakeWorldTestCase {
    _testDescription = "No release tag present"

  , _input = defaultInput {
      _iTags = [
        CiTag      $ UnixTimeVer 123
      ]
    }

  , _expectedOutput = initialOutput {
      _oCommands = [
        "checkout branch apples from tag ci/123"
      ]
    , _oLog = [
        "Cut release branch, apples"
      , "Could not find latest release tag"
      ]
  }
}


