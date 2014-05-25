module Program.Release (program) where

import Control.Applicative ((<$>))
import Control.Monad ((<=<))
import Control.Monad.Trans.Either (hoistEither, runEitherT)
import Control.Monad.Trans.Writer.Strict (WriterT, runWriterT, tell)
import Control.Monad.Trans.Class (lift)

import Types (
    Tag(..)
  , Branch(..)
  , Environment(..)
  )

import Interpreter.Commands (
    Program
  , EWP
  , deployTag
  , gitTags
  , gitCheckoutNewBranchFromTag
  , gitPushTags
  , gitTag
  )

import Tags (
    getNextReleaseCandidateTag
  , latestFilteredTag
  , releaseTagFilter
  , ciTagFilter
  )

program :: Program [String]
program = do
  -- strip WriterT by returning its accumulated logs
  ((), messages) <- runWriterT tellErrors
  return messages

  where
    tellErrors :: WriterT [String] Program ()
    tellErrors = do
      -- strip EitherT by just shoving the error message (in case of error) into the underlying writer transformer
      eitherResult <- runEitherT release
      case eitherResult of
        Right _ -> return ()
        Left err -> do
          tell [err]
          return ()

    release :: EWP ()
    release = do
      -- TODO: create a command for this
      {- releaseBranch <- Branch . head <$> liftIO getArgs -}
      let releaseBranch = Branch "bananas"

      cutReleaseBranch releaseBranch
      msg $ "Cut release branch, " ++ show releaseBranch

      releaseCandidateTag <- tagReleaseCandidate
      gitPushTags "origin" releaseBranch

      deployTag releaseCandidateTag Preproduction
      msg "Deployed to preproduction"

      msg $ "Release candidate " ++ show releaseCandidateTag ++ "/" ++ show releaseBranch ++ " has been deployed. Evaluate this release on http://preprod.gust.com."


      where
        msg message = lift $ tell [message]

        tagReleaseCandidate :: EWP Tag
        tagReleaseCandidate = do
          maybeTag <- (return . getNextReleaseCandidateTag <=< latestFilteredTag releaseTagFilter) <$> gitTags
          tag <- hoistEither $ maybeToEither "Could not find latest release tag" maybeTag
          gitTag tag
          return tag

        cutReleaseBranch :: Branch -> EWP ()
        cutReleaseBranch branch = do
          maybeTag <- latestFilteredTag ciTagFilter <$> gitTags
          tag <- hoistEither $ maybeToEither "Could not find latest green tag" maybeTag
          gitCheckoutNewBranchFromTag branch tag

        maybeToEither = flip maybe Right . Left
