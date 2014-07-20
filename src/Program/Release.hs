module Program.Release (program, determineReleaseState) where

import           Control.Applicative               ((<$>))
import           Control.Monad                     ((<=<))
import           Control.Monad.Trans.Class         (lift)
import           Control.Monad.Trans.Either        (hoistEither, runEitherT)
import           Control.Monad.Trans.Writer.Strict (WriterT, runWriterT, tell)
import           Data.Maybe                        (fromMaybe)

import           Types                             (Branch (..),
                                                    Environment (..),
                                                    ReleaseState (..), Tag (..),
                                                    Version (..))

import           Interpreter.Commands              (EWP, Program, deployTag,
                                                    getLineAfterPrompt,
                                                    gitCheckoutNewBranchFromTag,
                                                    gitCheckoutTag, gitPushTags,
                                                    gitRemoveTag, gitTag,
                                                    gitTags)

import           Tags                              (ciTagFilter,
                                                    defaultReleaseCandidateTag,
                                                    defaultReleaseTag,
                                                    getAllCandidatesForRelease,
                                                    getNextReleaseCandidateTag,
                                                    getReleaseTagFromCandidate,
                                                    isReleaseCandidateTag,
                                                    latestFilteredTag,
                                                    releaseCandidateTagFilter,
                                                    releaseTagFilter)



{- TODO: need to handle the case where one of these tags does not exist -}
determineReleaseState :: [Tag] -> ReleaseState
determineReleaseState tags =
  let
    latestReleaseCandidate = fromMaybe
      defaultReleaseCandidateTag $
      latestFilteredTag releaseCandidateTagFilter tags
    latestRelease = fromMaybe
      defaultReleaseTag $
      latestFilteredTag releaseTagFilter tags
  in
  if latestReleaseCandidate > latestRelease
    then ReleaseInProgress latestReleaseCandidate
    else NoReleaseInProgress latestRelease

program :: Program [String]
program = do
  -- strip WriterT by returning its accumulated logs
  snd <$> runWriterT tellErrors

  where
    tellErrors :: WriterT [String] Program ()
    tellErrors = do
      -- strip EitherT by just shoving the error message (in case of error) into the underlying writer transformer
      eitherResult <- runEitherT release
      either
        (tell . (:[]))
        return
        eitherResult

    release :: EWP ()
    release = do
      tags <- gitTags
      case determineReleaseState tags of
        ReleaseInProgress latestReleaseCandidate -> do
          msg $ "Release candidate found: " ++ (show latestReleaseCandidate)
          answer <- getLineAfterPrompt "Is this release candidate good? y(es)/n(o): "
          case answer of
            "y" -> releaseCandidate latestReleaseCandidate
            "n" -> do
              -- remove all RC tags for the corresponding upcoming release (e.g. 1.2.1-rc1, 1.2.1-rc2, etc)
              removeAllCandidateTagsForRelease $ getReleaseTagFromCandidate latestReleaseCandidate
              return ()
            _ -> return ()
        NoReleaseInProgress latestReleaseTag -> do
          -- checkout latest green build
          maybeTag <- latestFilteredTag ciTagFilter <$> gitTags
          lastGreenTag <- hoistEither $ maybeToEither "Could not find latest green tag" maybeTag
          gitCheckoutTag lastGreenTag
          -- tag next release candidate
          let releaseCandidateTag = getNextReleaseCandidateTag latestReleaseTag
          msg $ "No outstanding release candidates found, starting new release candidate: " ++ (show releaseCandidateTag)
          gitTag releaseCandidateTag
          -- push tags
          gitPushTags "origin"

      where
        removeAllCandidateTagsForRelease :: Tag -> EWP ()
        removeAllCandidateTagsForRelease releaseTag = do
          tags <- gitTags
          mapM_ gitRemoveTag $ getAllCandidatesForRelease releaseTag tags

        releaseCandidate latestReleaseCandidate = do
          gitCheckoutTag latestReleaseCandidate
          let releaseTag = getReleaseTagFromCandidate latestReleaseCandidate
          gitTag releaseTag
          gitPushTags "origin"
          msg $ "Created tag: " ++ (show releaseTag) ++ ", deploy to production cowboy!"

          {- cutReleaseBranch releaseBranch -}
          {- msg $ "Cut release branch, " ++ show releaseBranch -}

          {- releaseCandidateTag <- tagReleaseCandidate -}
          {- gitPushTags "origin" -}

          {- deployTag releaseCandidateTag Preproduction -}
          {- msg "Deployed to preproduction" -}

          {- msg $ "Release candidate " ++ -}
            {- (show releaseCandidateTag) ++ -}
            {- " on release branch " ++ -}
            {- (show releaseBranch) ++ -}
            {- " has been deployed. Evaluate this release on http://preprod.gust.com." -}

          where
            -- release/1.3.0-rc2/bugs/theres-a-bug-in-the-code
            {- releaseBranch = undefined -}

            tagReleaseCandidate :: EWP Tag
            tagReleaseCandidate = do
              maybeTag <- (return . getNextReleaseCandidateTag <=< latestFilteredTag releaseTagFilter) <$> gitTags
              tag <- hoistEither $ maybeToEither "Could not find latest release tag" maybeTag
              gitTag tag
              return tag

            {- cutReleaseBranch :: Branch -> EWP () -}
            {- cutReleaseBranch branch = do -}
              {- maybeTag <- latestFilteredTag ciTagFilter <$> gitTags -}
              {- tag <- hoistEither $ maybeToEither "Could not find latest green tag" maybeTag -}
              {- gitCheckoutNewBranchFromTag branch tag -}

        newCandidate latestRelease = undefined
          {- nextReleaseCandidateTag = getNextReleaseCandidateTag latestReleaseCandidate -}
          {- releaseCandidateTag <- tagReleaseCandidate -}

        msg message = lift $ tell [message]

        maybeToEither = flip maybe Right . Left
