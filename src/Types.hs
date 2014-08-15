{-# LANGUAGE OverloadedStrings #-}

module Types where

import           Data.List (intercalate)

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
  compare (SemVer majorX minorX _)  (SemVer majorY minorY _)  = 
    let majorComparison = majorX `compare` majorY in
    if majorComparison == EQ
    then minorX `compare` minorY
    else majorComparison
  compare _ _                             = error "no comparison"


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

data ReleaseState = NoReleaseInProgress Tag 
  | ReleaseInProgress Tag 
  | ReleaseInProgressBugfix Tag Branch 
  deriving (Eq, Show)

data ReleaseError = ExecutionError String | ProgramExpectationError String
instance Show ReleaseError where
  show (ExecutionError msg) = "Interpreter Error: " ++ msg
  show (ProgramExpectationError msg)     = "Program Error: " ++ msg
