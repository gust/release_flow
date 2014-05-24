module Tags where

import Data.List (sortBy)

import Types

getNextReleaseCandidateTag :: Tag -> Tag
getNextReleaseCandidateTag tag =
  ReleaseCandidateTag nextRelease 1
  where
    latestRelease = version tag
    nextRelease = nextMinorVersion latestRelease

    nextMinorVersion :: Version -> Version
    nextMinorVersion (SemVer major minor patch) = SemVer major (minor + 1) patch


latestFilteredTag :: (Tag -> Bool) -> [Tag] -> Tag
latestFilteredTag tagsFilter = head . reverseSort . filter tagsFilter
  where
    reverseSort :: Ord a => [a] -> [a]
    reverseSort = sortBy $ flip compare

releaseTagFilter :: Tag -> Bool
releaseTagFilter (ReleaseTag _) = True
releaseTagFilter _ = False

ciTagFilter :: Tag -> Bool
ciTagFilter (CiTag _) = True
ciTagFilter _ = False




