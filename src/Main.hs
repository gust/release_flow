{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative ((<$>))
import System.Environment (getArgs)
import Control.Monad.Trans.Either (runEitherT)
import Control.Monad.Trans.Writer.Strict (WriterT, runWriterT, tell)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)


import Types
import Tags
import External (
    gitDeployTag
  , gitGetTags
  , gitCheckoutNewBranchFromTag
  , gitPushTags
  , gitTag
  , gitFetchTags
  )


main :: IO ()
main = do
  ((), msg) <- runWriterT runStuff
  putStr $ "Log: " ++ msg

  where
    runStuff :: WriterT String IO ()
    runStuff = do
      eitherResult <- runEitherT release
      case eitherResult of
        Right _ -> return ()
        Left err -> do
          tell err
          return ()

    release :: EIO ()
    release = do
      releaseBranch <- Branch . head <$> liftIO getArgs

      gitFetchTags
      msg "Fetched tags"

      cutReleaseBranch releaseBranch
      msg $ "Cut release branch, " ++ show releaseBranch

      releaseCandidateTag <- tagReleaseCandidate
      gitPushTags "origin" releaseBranch

      gitDeployTag releaseCandidateTag Preproduction
      msg "Deployed to preproduction"

      msg $ "Release candidate " ++ show releaseCandidateTag ++ "/" ++ show releaseBranch ++ " has been deployed. Evaluate this release on http://preprod.gust.com."

      return()


      where
        msg message = lift $ tell $ message ++ "\n"

        tagReleaseCandidate :: EIO Tag
        tagReleaseCandidate = do
          tag <- getNextReleaseCandidateTag . latestFilteredTag releaseTagFilter <$> gitGetTags
          gitTag tag
          return tag

        cutReleaseBranch :: Branch -> EIO ()
        cutReleaseBranch branch = do
          tag <- latestFilteredTag ciTagFilter <$> gitGetTags
          gitCheckoutNewBranchFromTag branch tag


