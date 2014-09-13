module Integration.TestCases.Common
  (
    jackShit
  , noReleaseInProgressStartRelease
  , noReleaseInProgressStartHotfix
  , releaseInProgressGoodMinorRelease
  , releaseInProgressGoodPatchRelease
  , releaseInProgressBad
  , releaseInProgressBugFoundBugIsFixed
  , releaseInProgressBugFoundBugIsNotFixed
  , hotfixInProgressHotfixFixed
  , hotfixInProgressHotfixNotFixed
  , FakeWorldTestCase
  )
  where


import           Interpreter.State (Input (..), Output (..), World (..),
                                    defaultInput, defaultWorld, initialOutput,
                                    interpret)
import           Types             (Branch (..), Tag (..), Version (..))


data FakeWorldTestCase = FakeWorldTestCase {
    _testDescription :: String
  , _input           :: Input
  , _expectedOutput  :: Output
  }

jackShit = FakeWorldTestCase {
    _testDescription = "Nothing"
  , _input = defaultInput {
      _iUserInput = [
        ("Choose your adventure: Start new release (0), Start hotfix (1)", "0")
      ]
    }
  , _expectedOutput = initialOutput {
      _oCommands = [
          "git tags"
        , "git branch"
        ]
    , _oStdOut  = []
    , _oStdErr  = ["Program Error: Could not find latest green tag"]
    }
  }

noReleaseInProgressInput = defaultInput {
    _iTags = [
      ReleaseCandidateTag (SemVer 1 2 3) 3
    , ReleaseTag $ SemVer 1 2 3
    , CiTag      $ UnixTimeVer 123
    ]
  }

hotfixInProgressInput = defaultInput {
    {- TODO, we may want to add some more tags here to replicate edge cases -}
    _iTags = [
      ReleaseTag $ SemVer 1 2 3
    ]
  , _iBranches = [
      Branch "release/1.2.3/hotfix/hot-fixing"
    ]
}

noReleaseInProgressStartHotfix = FakeWorldTestCase {
    _testDescription = "No release in progress, start a hotfix"

  , _input = noReleaseInProgressInput {
      _iUserInput = [
        ("Choose your adventure: Start new release (0), Start hotfix (1)", "1")
      , ("What is the hotfix for? (specify dash separated descriptor, e.g. 'signup-is-broken')", "hot-fixing")
      ]
    }

  , _expectedOutput = initialOutput {
      _oCommands = [
          "git tags"
        , "git branch"
        , "git checkout release/1.2.3"
        , "git checkout -b release/1.2.3/hotfix/hot-fixing"
        ]
    , _oStdOut  = [
          "Started hotfix: release/1.2.3/hotfix/hot-fixing, fix stuff!"
        ]
    , _oStdErr = [ ]
  }
}

hotfixInProgressHotfixNotFixed = FakeWorldTestCase {
    _testDescription = "Hotfix in progress, not yet completed"

  , _input = hotfixInProgressInput {
      _iUserInput = [
        ("Is the hotfix complete? y(es)/n(o)", "n")
      ]
    }

  , _expectedOutput = initialOutput {
      _oCommands = [
          "git tags"
        , "git branch"
        , "git checkout release/1.2.3/hotfix/hot-fixing"
        ]
    , _oStdOut  = [
          "Hotfix found: release/1.2.3/hotfix/hot-fixing"
        , "Keep fixing that code!"
        ]
    , _oStdErr = [ ]
  }
}

hotfixInProgressHotfixFixed = FakeWorldTestCase {
    _testDescription = "Hotfix in progress, completed"

  , _input = hotfixInProgressInput {
      _iUserInput = [
        ("Is the hotfix complete? y(es)/n(o)", "y")
      ]
    }

  , _expectedOutput = initialOutput {
      _oCommands = [
          "git tags"
        , "git branch"
        , "git checkout release/1.2.3/hotfix/hot-fixing"
        , "git tag release/1.2.4-rc1"
        , "git push origin --tags"
        , "git checkout release/1.2.4-rc1"
        , "git branch -d release/1.2.3/hotfix/hot-fixing"
        , "git push origin :release/1.2.3/hotfix/hot-fixing"
        ]
    , _oStdOut  = [
          "Hotfix found: release/1.2.3/hotfix/hot-fixing"
        , "Started new release: release/1.2.4-rc1, deploy to preproduction and confirm the release is good to go!"
        ]
    , _oStdErr = [ ]
  }
}

noReleaseInProgressStartRelease = FakeWorldTestCase {
    _testDescription = "No release in progress, start a new release"

  , _input = noReleaseInProgressInput {
      _iUserInput = [
        ("Choose your adventure: Start new release (0), Start hotfix (1)", "0")
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
          "Started new release: release/1.3.0-rc1, deploy to preproduction and confirm the release is good to go!"
        ]
    , _oStdErr = [ ]
    }
}

releaseInProgressGoodMinorRelease = FakeWorldTestCase {
    _testDescription = "Release in progress, candidate is good, candidate is minor version"

  , _input = defaultInput {
      _iTags = [
        ReleaseCandidateTag (SemVer 1 3 0) 2
      , ReleaseTag $ SemVer 1 2 3
      , CiTag      $ UnixTimeVer 123
      ]
    , _iUserInput = [
        ("Is this release candidate good? y(es)/n(o)", "y")
      ]
    }

  , _expectedOutput = initialOutput {
      _oCommands = [
          "git tags"
        , "git branch"
        , "git checkout release/1.3.0-rc2"
        , "git tag release/1.3.0"
        , "git push origin --tags"
        , "git checkout release/1.3.0"
        ]
    , _oStdOut  = [
          "Release candidate found: release/1.3.0-rc2"
        , "Created tag: release/1.3.0, deploy to production cowboy!"
        ]
    , _oStdErr = [ ]
  }
}

releaseInProgressGoodPatchRelease = FakeWorldTestCase {
    _testDescription = "Release in progress, candidate is good, candidate is patch (hotfix)"

  , _input = defaultInput {
      _iTags = [
        ReleaseCandidateTag (SemVer 1 2 4) 2
      , ReleaseTag $ SemVer 1 2 3
      , CiTag      $ UnixTimeVer 123
      ]
    , _iUserInput = [
        ("Is this release candidate good? y(es)/n(o)", "y")
      ]
    }

  , _expectedOutput = initialOutput {
      _oCommands = [
          "git tags"
        , "git branch"
        , "git checkout release/1.2.4-rc2"
        , "git tag release/1.2.4"
        , "git push origin --tags"
        , "git checkout integration"
        , "git pull --rebase"
        , "git merge --no-ff release/1.2.4"
        , "git push origin integration"
        , "git checkout release/1.2.4"
        ]
    , _oStdOut  = [
          "Release candidate found: release/1.2.4-rc2"
        , "Created tag: release/1.2.4, deploy to production cowboy!"
        ]
    , _oStdErr = [ ]
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
        ("Is this release candidate good? y(es)/n(o)", "n")
      , ("What bug are you fixing? (specify dash separated descriptor, e.g. 'theres-a-bug-in-the-code')", "theres-a-bug-in-the-code")
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
    , _oStdErr = [ ]
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
        ("Is the bug fixed? y(es)/n(o)", "y")
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
        , "git checkout release/1.3.0-rc3"
        , "git branch -d release/1.3.0-rc2/tmp"
        , "git push origin :release/1.3.0-rc2/tmp"
        ]
    , _oStdOut  = [
          "Bugfix found: release/1.3.0-rc2/bugs/theres-a-bug-in-the-code"
        , "Created new release candidate: release/1.3.0-rc3, you'll get it this time!"
        ]
    , _oStdErr = [ ]
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
        ("Is the bug fixed? y(es)/n(o)", "n")
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
    , _oStdErr = [ ]
  }
}

