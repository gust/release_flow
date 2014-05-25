{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative ((<$>))
import System.Environment (getArgs)
import Control.Monad ((<=<))
import Control.Monad.Trans.Either (hoistEither, runEitherT)
import Control.Monad.Trans.Writer.Strict (WriterT, runWriterT, tell)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)

import Types
import Commands
import Interpreter (interpret, EIO)
import Tags

main :: IO ()
main = do
  eitherResult <- runEitherT $ interpret program
  case eitherResult of
    Right messages -> do
      logError $ "Log: " ++ messages
      return ()
    Left err -> do
      logError $ "Error: " ++ err
      return ()

  where
    logError :: String -> IO ()
    logError = putStrLn


program :: Program String
program = do
  ((), messages) <- runWriterT tellErrors
  return messages

  where
    tellErrors :: WriterT String Program ()
    tellErrors = do
      eitherResult <- runEitherT release
      case eitherResult of
        Right _ -> return ()
        Left err -> do
          tell err
          return ()

    release :: EWP ()
    release = do
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
        msg message = lift $ tell $ message ++ "\n"

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
