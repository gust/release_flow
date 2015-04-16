{-# LANGUAGE OverloadedStrings #-}

module Types where

import           Data.List (intercalate)
import           System.Console.ANSI

releaseTagPrefix = "release/"
ciTagPrefix = "ci/"

prefix :: Tag -> String
prefix (ReleaseTag _)             = releaseTagPrefix
prefix (CiTag _)                  = ciTagPrefix
prefix (ReleaseCandidateTag _ _)  = releaseTagPrefix

tmpBranch :: Tag -> Branch
tmpBranch tag = Branch $ (show tag) ++ "/tmp"

data Version = SemVer {
    major :: Int
  , minor :: Int
  , patch :: Int
  } | UnixTimeVer {
    timestamp :: Int
  } deriving (Eq)

instance Show Version where
  show (SemVer major minor patch) = intercalate "." [(show major), (show minor), (show patch)]
  show (UnixTimeVer timestamp)    = show timestamp

instance Ord Version where
  compare (UnixTimeVer x) (UnixTimeVer y) = x `compare` y
  compare (SemVer majorX minorX patchX)  (SemVer majorY minorY patchY) = [majorX, minorX, patchX] `compare` [majorY, minorY, patchY]
  compare _ _                             = error "no comparison"

(SemVer a b c) `isNextPatchOf` (SemVer d e f)
  | a /= d     = False
  | b /= e     = False
  | c == f + 1 = True
  | otherwise  = False

data Tag = ReleaseTag {
    version :: Version
  } |
  ReleaseCandidateTag {
    version :: Version
  , rc      :: Int
  } |
  CiTag {
    version :: Version
  } deriving (Eq)


instance Show Tag where
  show tag@(ReleaseTag ver)              = prefix(tag) ++ (show ver)
  show tag@(CiTag ver)                   = prefix(tag) ++ (show ver)
  show tag@(ReleaseCandidateTag ver rc)  = prefix(tag) ++ (show ver) ++ "-rc" ++ (show rc)

instance Ord Tag where
  compare (ReleaseTag a) (ReleaseCandidateTag b rcb) = a `compare` b
  compare (ReleaseCandidateTag a rca) (ReleaseTag b) = a `compare` b
  compare (ReleaseCandidateTag a rca) (ReleaseCandidateTag b rcb) = if a `compare` b == EQ then rca `compare` rcb else a `compare` b
  compare (ReleaseTag a) (ReleaseTag b) = a `compare` b
  compare (CiTag a) (CiTag b) = a `compare` b
  compare _ _ = error "no comparison possible between tags"

data Branch = Branch {branchName :: String} deriving (Eq)
instance Show Branch where
  show = branchName

data Environment = Preproduction | Production deriving (Eq)
instance Show Environment where
  show Preproduction  = "preproduction"
  show Production     = "production"

data ReleaseState = HotfixInProgress Tag Branch
  | ReleaseInProgressBugfix Tag Branch
  | ReleaseInProgress Tag Tag
  | NoReleaseInProgress Tag
  deriving (Eq, Show)

data ReleaseError = ExecutionError String | ProgramExpectationError String
instance Show ReleaseError where
  show (ExecutionError msg) = "Interpreter Error: " ++ msg
  show (ProgramExpectationError msg)     = "Program Error: " ++ msg

data Message = Message MessageType String
data MessageType = Command
  | Info
  | Prompt

instance Show Message where
  show (Message messageType string) = withColor (colorFor messageType) string where
    withColor :: [SGR] -> String -> String
    withColor color string = (setSGRCode color) ++ string ++ resetCode

    colorFor :: MessageType -> [SGR]
    colorFor Command = [SetColor Foreground Vivid Black]
    colorFor Info    = [SetColor Foreground Dull Green]
    colorFor Prompt  = [SetColor Foreground Vivid Yellow]

    resetCode = "\x1b[0m"

infoMessage = Message Info
promptMessage = Message Prompt
commandMessage = Message Command
