module Integration.TestCases (integrationTestCases) where

type Command = String

data SpecInput = SpecInput {
      siTags        :: [Tag]
    , siUserChoices :: (String, String)
  }

data SpecOutput = SpecOutput {
      siCommands :: [Command]
  }

data Spec = Spec {
      sInput  :: SpecInput
    , sOutput :: SpecOutput
    , sStdout :: [String]
  }

data TestCase = TestCase {
      tcName :: String
    , tcSpec :: Spec
  }

data TestCaseTree = TestCaseTree [TestCaseTree] | TestCaseNode TestCase



instance FromJSON TestCase where
  parseJSON (Object v) = TestCase <$>
                         v .: "name" <*>
                         v .: "spec"
  -- A non-Object value is of the wrong type, so fail.
  parseJSON _ = error "Can't parse TestCase from YAML/JSON"
