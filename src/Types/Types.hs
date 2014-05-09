{-# LANGUAGE OverloadedStrings #-}

module Types.Types where

import Data.List (intercalate)

releaseTagPrefix = "release/"
ciTagPrefix = "ci/"

prefix :: Tag -> String
prefix (ReleaseTag _)           = releaseTagPrefix
prefix (CiTag _)                = ciTagPrefix
prefix (ReleaseCandidateTag _ _)  = releaseTagPrefix

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
  compare (SemVer x _ _)  (SemVer y _ _)  = x `compare` y
  compare _ _                             = error "no comparison"


data Tag = ReleaseTag {
    version   :: Version
  } |
  ReleaseCandidateTag {
    version   :: Version
  , rc        :: Int
  } |
  CiTag {
    version   :: Version
  } deriving (Eq)

instance Show Tag where
  show tag@(ReleaseTag version)              = prefix(tag) ++ (show version)
  show tag@(CiTag version)                   = prefix(tag) ++ (show version)
  show tag@(ReleaseCandidateTag version rc)  = prefix(tag) ++ (show version) ++ "-rc" ++ (show rc)

instance Ord Tag where
  compare (ReleaseTag a) (ReleaseTag b) = a `compare` b
  compare (CiTag a) (CiTag b) = a `compare` b
  compare _ _ = error "no comparison"


