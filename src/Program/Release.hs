module Program.Release (program) where

import           Control.Applicative               ((<$>))
import           Control.Monad                     ((<=<))
import           Control.Monad.Trans.Class         (lift)
import           Control.Monad.Trans.Either        (hoistEither, runEitherT)
import           Control.Monad.Trans.Writer.Strict (WriterT, runWriterT, tell)

import           Types                             (Branch (..),
                                                    Environment (..), Tag (..))

import           Interpreter.Commands              (EWP, Program, deployTag,
                                                    , gitCheckoutTag
                                                    , gitCheckoutNewBranchFromTag
                                                    , gitPushTags
                                                    , gitTag
                                                    , gitTags
                                                    )

import           Tags                              (ciTagFilter,
                                                    getNextReleaseCandidateTag,
                                                    latestFilteredTag,
                                                    releaseTagFilter)


data ReleaseState = NoReleaseInProgress Tag | ReleaseInProgress Tag

{- TODO: need to handle the case where one of these tags does not exist -}
determineReleaseState :: [Tag] -> ReleaseState
determineReleaseState tags =
  latestReleaseCandidate = latestFilteredTag releaseCandidateTagFilter tags
  latestRelease = latestFilteredTag releaseTagFilter tags
  case latestReleaseCandidate > latestRelease of
    True -> ReleaseInProgress latestReleaseCandidate
    False -> NoReleaseInProgress latestRelease

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
          releaseTheCandidate latestReleaseCandidate
        NoReleaseInProgress latestRelease -> newCandidate latestRelease

      where
        releaseTheCandidate latestReleaseCandidate = do
          gitCheckoutTag latestReleaseCandidate
          gitTag $ releaseTag latestReleaseCandidate
          gitPushTags
          msg "Created tag: " ++ show releaseTag ++ ", deploy to production cowboy!"



          {- cutReleaseBranch releaseBranch -}
          {- msg $ "Cut release branch, " ++ show releaseBranch -}

          releaseCandidateTag <- tagReleaseCandidate
          gitPushTags "origin" releaseBranch

          deployTag releaseCandidateTag Preproduction
          msg "Deployed to preproduction"

          msg $ "Release candidate " ++ show releaseCandidateTag ++ " on release branch " ++ show releaseBranch ++ " has been deployed. Evaluate this release on http://preprod.gust.com."


          where
            msg message = lift $ tell [message]

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

            releaseTag (ReleaseCandidateTag v _) = ReleaseTag v

            maybeToEither = flip maybe Right . Left


        newCandidate latestRelease = undefined
          {- nextReleaseCandidateTag = getNextReleaseCandidateTag latestReleaseCandidate -}
          {- releaseCandidateTag <- tagReleaseCandidate -}
