module Tags (
    releaseCandidateTagFilter
  , getNextReleaseCandidateTag
  , releaseTagFilter
  , ciTagFilter
  , latestFilteredTag
  , defaultReleaseCandidateTag
  , defaultReleaseTag
  , getReleaseTagFromCandidate
  , isReleaseCandidateTag
  , getAllCandidatesForRelease
  , getNextMinorReleaseCandidateTag
  , getNextPatchReleaseCandidateTag
  )
  where

import           Data.List  (sortBy)
import           Data.Maybe (listToMaybe)

import           Types

nextMinorVersion :: Version -> Version
nextMinorVersion (SemVer major minor patch) = SemVer major (minor + 1) 0
nextMinorVersion version = error $ "Concept unsupported for " ++ (show version)

nextPatchVersion :: Version -> Version
nextPatchVersion (SemVer major minor patch) = SemVer major minor (patch + 1)
nextPatchVersion version = error $ "Concept unsupported for " ++ (show version)

defaultReleaseCandidateTag = ReleaseCandidateTag (SemVer 0 0 0) 0
defaultReleaseTag = ReleaseTag $ SemVer 0 0 0

getNextReleaseCandidateTag :: (Version -> Version) -> Tag -> Tag
getNextReleaseCandidateTag _                (ReleaseCandidateTag version rc) = ReleaseCandidateTag version (rc + 1)
getNextReleaseCandidateTag incrementVersion (ReleaseTag version)             = ReleaseCandidateTag (incrementVersion version) 1

getNextMinorReleaseCandidateTag = getNextReleaseCandidateTag nextMinorVersion
getNextPatchReleaseCandidateTag = getNextReleaseCandidateTag nextPatchVersion

latestFilteredTag :: (Tag -> Bool) -> [Tag] -> Maybe Tag
latestFilteredTag tagsFilter = listToMaybe . reverseSort . filter tagsFilter
  where
    reverseSort :: Ord a => [a] -> [a]
    reverseSort = sortBy $ flip compare

releaseCandidateTagFilter :: Tag -> Bool
releaseCandidateTagFilter (ReleaseCandidateTag _ _) = True
releaseCandidateTagFilter _ = False

releaseTagFilter :: Tag -> Bool
releaseTagFilter (ReleaseTag _) = True
releaseTagFilter _ = False

ciTagFilter :: Tag -> Bool
ciTagFilter (CiTag _) = True
ciTagFilter _ = False

getReleaseTagFromCandidate (ReleaseCandidateTag v _) = ReleaseTag v

isReleaseCandidateTag (ReleaseCandidateTag _ _) = True
isReleaseCandidateTag _ = False

getAllCandidatesForRelease :: Tag -> [Tag] -> [Tag]
getAllCandidatesForRelease releaseTag =
  filter (\tag -> isReleaseCandidateTag tag && (releaseTag == getReleaseTagFromCandidate tag) )
