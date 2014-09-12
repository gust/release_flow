module Integration.TestCases (integrationTestCases) where

type Command = String

data SpecInput = SpecInput {
      siTags    :: [Tag]
    , siPrompts :: (String, String)
  }

data SpecOutput = SpecOutput {
      sCommands :: [Command]
    , sStdout   :: [String]
    , sStderr   :: [String]
  }

data Spec = Spec {
      sInput    :: SpecInput
    , sOutput   :: SpecOutput
  }

data TestCase = TestCase {
      tcName    :: String
    , tcSpec    :: Spec
    , tcTestCases :: [TestCase]
  }



instance FromJSON TestCase where
  parseJSON (Object v) = TestCase <$>
                         v .: "name" <*>
                         v .: "spec" <*>
                         v .: "tests"
  -- A non-Object value is of the wrong type, so fail.
  parseJSON _ = error "Can't parse TestCase from YAML/JSON"

instance FromJSON Spec where
  parseJSON (Object v) = Spec <$>
                         v .: "input" <*>
                         v .: "output" <*>

  -- A non-Object value is of the wrong type, so fail.
  parseJSON _ = error "Can't parse Spec from YAML/JSON"

instance FromJSON SpecInput where
  parseJSON (Object v) = Spec <$>
                         v .: "tags" <*>
                         v .: "prompts"
  -- A non-Object value is of the wrong type, so fail.
  parseJSON _ = error "Can't parse SpecInput from YAML/JSON"

instance FromJSON SpecOutput where
  parseJSON (Object v) = Spec <$>
                         v .: "commands" <*>
                         v .: "stdout" <*>
                         v .: "stderr"
  -- A non-Object value is of the wrong type, so fail.
  parseJSON _ = error "Can't parse SpecOutput from YAML/JSON"
