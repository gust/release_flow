module Program.Release (program) where

import           Control.Applicative               ((<$>))
import           Control.Monad                     ((<=<))
import           Control.Monad.Trans.Class         (lift)
import           Control.Monad.Trans.Either        (hoistEither, runEitherT)
import           Control.Monad.Trans.Writer.Strict (WriterT, runWriterT, tell)
import           Data.Maybe                        (fromMaybe)

import           Types                             (Branch (..),
                                                    Environment (..), Tag (..),
                                                    Version (..))

import           Interpreter.Commands              (EWP, Program, deployTag,
                                                    gitCheckoutNewBranchFromTag,
                                                    gitCheckoutTag, gitPushTags,
                                                    gitTag, gitTags)

import           Tags                              (ciTagFilter,
                                                    defaultReleaseCandidateTag,
                                                    defaultReleaseTag,
                                                    getNextReleaseCandidateTag,
                                                    getReleaseTagFromCandidate,
                                                    latestFilteredTag,
                                                    releaseCandidateTagFilter,
                                                    releaseTagFilter)


data ReleaseState = NoReleaseInProgress Tag | ReleaseInProgress Tag



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
          msg $ "Release candidate found: " ++ show latestReleaseCandidate
          {- TODO prompt is release candidate good? -}
          releaseCandidate latestReleaseCandidate
        NoReleaseInProgress latestReleaseTag -> do
          -- checkout latest green build
          maybeTag <- latestFilteredTag ciTagFilter <$> gitTags
          tag <- hoistEither $ maybeToEither "Could not find latest green tag" maybeTag
          -- tag next release candidate
          gitTag $ getNextReleaseCandidateTag latestReleaseTag
          -- push tags
          gitPushTags "origin"

      where
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

        releaseCandidate latestReleaseCandidate = do
          gitCheckoutTag latestReleaseCandidate
          let releaseTag = getReleaseTagFromCandidate latestReleaseCandidate
          gitTag releaseTag
          gitPushTags "origin"
          msg $ "Created tag: " ++ (show releaseTag) ++ ", deploy to production cowboy!"

          {- cutReleaseBranch releaseBranch -}
          {- msg $ "Cut release branch, " ++ show releaseBranch -}

          releaseCandidateTag <- tagReleaseCandidate
          gitPushTags "origin"

          deployTag releaseCandidateTag Preproduction
          msg "Deployed to preproduction"

          msg $ "Release candidate " ++
            (show releaseCandidateTag) ++
            {- " on release branch " ++ -}
            {- (show releaseBranch) ++ -}
            " has been deployed. Evaluate this release on http://preprod.gust.com."

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
