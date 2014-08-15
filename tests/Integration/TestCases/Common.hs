module Integration.TestCases.Common
  (
    jackShit
  , noReleaseInProgress
  , releaseInProgressGood
  , releaseInProgressBad
  , releaseInProgressBugFoundBugIsFixed
  , releaseInProgressBugFoundBugIsNotFixed
  , FakeWorldTestCase
  )
  where


import           Interpreter.State (Input (..), Output (..), World (..),
                                    defaultInput, defaultWorld, initialOutput,
                                    interpret)
import           Types             (Tag (..), Version (..), Branch(..))


data FakeWorldTestCase = FakeWorldTestCase {
    _testDescription :: String
  , _input           :: Input
  , _expectedOutput  :: Output
  }

jackShit = FakeWorldTestCase {
    _testDescription = "Nothing"
  , _input = defaultInput
  , _expectedOutput = initialOutput {
        _oCommands = [
            "git tags"
          , "git branch"
          ]
      , _oStdOut  = []
      , _oLog     = ["Could not find latest green tag"]
    }
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
        , "git branch"
        , "git checkout ci/123"
        , "git tag release/1.3.0-rc1"
        , "git push origin --tags"
        ]
    , _oStdOut  = [
          "No outstanding release candidate found, starting new release candidate from: ci/123"
        , "Started new release: release/1.3.0-rc1, deploy to preproduction and confirm the release is good to go!"
        ]
    , _oLog = [ ]
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
        ("Is this release candidate good? y(es)/n(o):", "y")
      ]
    }

  , _expectedOutput = initialOutput {
      _oCommands = [
          "git tags"
        , "git branch"
        , "git checkout release/1.3.0-rc2"
        , "git tag release/1.3.0"
        , "git push origin --tags"
        ]
    , _oStdOut  = [
          "Release candidate found: release/1.3.0-rc2"
        , "Created tag: release/1.3.0, deploy to production cowboy!"
        ]
    , _oLog = [ ]
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
        ("Is this release candidate good? y(es)/n(o):", "n")
      , ("What bug are you fixing? (specify dash separated descriptor, e.g. 'theres-a-bug-in-the-code'): ", "theres-a-bug-in-the-code")
      ]
    }

  , _expectedOutput = initialOutput {
      _oCommands = [
          "git tags"
        , "git branch"
        , "git checkout release/1.3.0-rc2"
        , "git checkout -b release/1.3.0-rc2/tmp"
        , "git checkout -b release/1.3.0-rc2/bugs/theres-a-bug-in-the-code"
        ]
    , _oStdOut  = [
          "Release candidate found: release/1.3.0-rc2"
        , "Created branch: release/1.3.0-rc2/bugs/theres-a-bug-in-the-code, fix your bug!"
        ]
    , _oLog = [ ]
  }
}

releaseInProgressBugFoundBugIsFixed = FakeWorldTestCase {
    _testDescription = "Release in progress, bug fix in progress, bug fix complete"

  , _input = defaultInput {
      _iTags = [
        ReleaseCandidateTag (SemVer 1 3 0) 2
      ]
    , _iBranches = [
        Branch "release/1.3.0-rc2/bugs/theres-a-bug-in-the-code"
      ]
    , _iUserInput = [
        ("Is the bug fixed? y(es)/n(o):", "y")
      ]
    }

  , _expectedOutput = initialOutput {
      _oCommands = [
          "git tags"
        , "git branch"
        , "git checkout release/1.3.0-rc2/tmp"
        , "git merge --no-ff release/1.3.0-rc2/bugs/theres-a-bug-in-the-code"
        , "git branch -d release/1.3.0-rc2/bugs/theres-a-bug-in-the-code"
        , "git push origin :release/1.3.0-rc2/bugs/theres-a-bug-in-the-code"
        , "git tag release/1.3.0-rc3"
        , "git push origin --tags"
        , "git branch -d release/1.3.0-rc2/tmp"
        , "git push origin :release/1.3.0-rc2/tmp"
        ]
    , _oStdOut  = [
          "Bugfix found: release/1.3.0-rc2/bugs/theres-a-bug-in-the-code"
        , "Created new release candidate: release/1.3.0-rc3, you'll get it this time!"
        ]
    , _oLog = [ ]
  }
}

releaseInProgressBugFoundBugIsNotFixed = FakeWorldTestCase {
    _testDescription = "Release in progress, bug fix in progress, bug fix not complete"

  , _input = defaultInput {
      _iTags = [
        ReleaseCandidateTag (SemVer 1 3 0) 2
      ]
    , _iBranches = [
        Branch "release/1.3.0-rc2/bugs/theres-a-bug-in-the-code"
      ]
    , _iUserInput = [
        ("Is the bug fixed? y(es)/n(o):", "n")
      ]
    }

  , _expectedOutput = initialOutput {
      _oCommands = [
          "git tags"
        , "git branch"
        , "git checkout release/1.3.0-rc2/bugs/theres-a-bug-in-the-code"
        ]
    , _oStdOut  = [
          "Bugfix found: release/1.3.0-rc2/bugs/theres-a-bug-in-the-code"
        , "Keep fixing that code!"
        ]
    , _oLog = [ ]
  }
}
