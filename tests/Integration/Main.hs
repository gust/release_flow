{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}


module Integration.Main where

import           Test.Tasty                 (TestTree, testGroup)
import           Test.Tasty.HUnit           (testCase, (@?=))

import           Control.Monad.State.Strict (State, get, put, runState)
import           Control.Monad.Trans.Either (EitherT, runEitherT)
import           Data.List                  (intercalate)
import Data.Monoid
import Data.Text (unpack)
import Data.Maybe (fromMaybe)
import Control.Applicative ((<$>), (<*>), pure)
import Data.Yaml (decode)
import Data.Aeson (FromJSON(..), Value(..), (.:), (.:?))
import           Control.Lens               (makeLenses, (%=), (^.))
import           Interpreter.State          (Input (..), Output (..),
                                             World (..), defaultInput,
                                             defaultWorld, initialOutput,
                                             interpret)
import           Program.Release            (program, runProgram)
import           Types                      (Branch (..), Tag (..), Version (..))



import Parser.Tag (tagParser)
import Text.ParserCombinators.Parsec (parse)

type Command = String

data TestCase = TestCase {
    _tcName      :: String
  , _tcTestCases :: Maybe [TestCase]
  , _tcSpec      :: Maybe Spec
  } deriving Show

data Spec = Spec {
    _sInput      :: Maybe SpecInput
  , _sOutput     :: Maybe SpecOutput
  } deriving (Eq, Show)

data SpecInput = SpecInput {
    _sTags       :: Maybe [Tag]
  , _sBranches   :: Maybe [Branch]
  , _sPrompts    :: Maybe [Prompt]
  } deriving (Eq, Show)

data Prompt = Prompt {
    _pQuestion   :: String
  , _pAnswer     :: String
  } deriving (Eq, Show)

data SpecOutput = SpecOutput {
    _sCommands   :: Maybe [Command]
  , _sStdOut     :: Maybe [String]
  , _sStdErr     :: Maybe [String]
  } deriving (Eq, Show)

instance Monoid Spec where
  mempty = Spec Nothing Nothing
  Spec i1 _ `mappend` Spec i2 o = Spec (i1 `deepAppend` i2) o 

instance Monoid SpecInput where
  mempty = SpecInput Nothing Nothing Nothing
  SpecInput t1 b1 p1 `mappend` SpecInput t2 b2 p2 = 
    SpecInput 
      (t1 `deepAppend` t2) 
      (b1 `deepAppend` b2) 
      (p1 `deepAppend` p2) 

deepAppend :: Monoid a => Maybe a -> Maybe a -> Maybe a
deepAppend (Just a1) (Just a2) = Just $ a1 <> a2
deepAppend maybe1 maybe2 = maybe1 <> maybe2

instance FromJSON TestCase where
  parseJSON (Object v) = TestCase  
                          <$> v .:  "name" 
                          <*> v .:? "tests"
                          <*> v .:? "spec" 
-- A non-Object value is of the wrong type, so fail.
  parseJSON _ = error "Can't parse TestCase from YAML/JSON"


instance FromJSON Spec where
  parseJSON (Object v) = Spec 
                          <$>  v .:? "input" 
                          <*>  v .:? "output"
  -- A non-Object value is of the wrong type, so fail.
  parseJSON _ = error "Can't parse Spec from YAML/JSON"


instance FromJSON SpecInput where
  parseJSON (Object v) = SpecInput 
                          <$>  v .:? "tags"
                          <*>  v .:? "branches"
                          <*>  v .:? "prompts"
  -- A non-Object value is of the wrong type, so fail.
  parseJSON _ = error "Can't parse SpecInput from YAML/JSON"


instance FromJSON Prompt where
  parseJSON (Object v) = Prompt 
                          <$>  v .: "question"
                          <*>  v .: "answer"
  -- A non-Object value is of the wrong type, so fail.
  parseJSON _ = error "Can't parse Prompt from YAML/JSON"


instance FromJSON SpecOutput where
  parseJSON (Object v) = SpecOutput 
                          <$>  v .:? "commands"
                          <*>  v .:? "stdout"
                          <*>  v .:? "stderr"
  -- A non-Object value is of the wrong type, so fail.
  parseJSON _ = error "Can't parse SpecOutput from YAML/JSON"


instance FromJSON Tag where
  parseJSON (String s) = case parse tagParser "" (unpack s) of 
    Right (Just tag) -> pure tag 
    Left _ -> fail "bleh"
  -- A non-Object value is of the wrong type, so fail.
  parseJSON _ = error "Can't parse Tag from YAML/JSON"

instance FromJSON Branch where
  parseJSON (String s) = pure $ Branch $ unpack s
  -- A non-Object value is of the wrong type, so fail.
  parseJSON _ = error "Can't parse Branch from YAML/JSON"


makeLenses ''Input
makeLenses ''Output
makeLenses ''World

makeLenses ''TestCase
makeLenses ''Spec
makeLenses ''SpecInput
makeLenses ''SpecOutput
makeLenses ''Prompt

toInput :: Maybe SpecInput -> Input
toInput Nothing = defaultInput
toInput (Just si) = Input{
    _iTags = fromMaybe [] $ si^.sTags
  , _iBranches = fromMaybe [] $ si^.sBranches
  , _iUserInput = map (\p -> (p^.pQuestion, p^.pAnswer)) $ fromMaybe [] $ si^.sPrompts
  }

toOutput :: Maybe SpecOutput -> Output
toOutput Nothing = initialOutput
toOutput (Just so) = Output{
    _oCommands  = fromMaybe [] $ so^.sCommands
  , _oStdOut    = fromMaybe [] $ so^.sStdOut
  , _oStdErr    = fromMaybe [] $ so^.sStdErr
  }


integrationTestTree :: TestCase -> TestTree
integrationTestTree = inheritedTestTree mempty

inheritedTestTree :: Spec -> TestCase -> TestTree

inheritedTestTree _ tc@TestCase{_tcTestCases = Nothing, _tcSpec = Nothing } = 
  failAssertion "Test case must have a spec or be a group of test cases"

inheritedTestTree parentSpec tc@TestCase{_tcTestCases = Just tcs,     _tcSpec = Nothing} = 
  testGroup (tc^.tcName) $ map (inheritedTestTree parentSpec) tcs

inheritedTestTree parentSpec tc@TestCase{_tcTestCases = Just tcs,     _tcSpec = (Just spec)} = 
  testGroup (tc^.tcName) $ map (inheritedTestTree (parentSpec <> spec)) tcs

inheritedTestTree parentSpec tc@TestCase{_tcTestCases = Nothing, _tcSpec = Just spec} = testCase (tc^.tcName) $
  (run (toInput $ (parentSpec <> spec)^.sInput)) @?= (toOutput $ spec^.sOutput)
  where
    run :: Input -> Output
    run input = output
      where
        output = _wOutput endWorld
          where
            ((), endWorld) = runState stateInterpretation startWorld
              where
                startWorld = defaultWorld {
                  _wInput = input
                }

                stateInterpretation :: State World ()
                stateInterpretation = runProgram interpret program >>= either (logError . show) return

                logError :: String -> State World ()
                logError msg = wOutput.oStdErr %= (\existing -> existing ++ [msg])

failAssertion s = testCase s $ 1 @?= 2
