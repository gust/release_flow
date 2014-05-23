{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative ((<$>))
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))

import Text.ParserCombinators.Parsec

import System.Environment (getArgs)
import Data.List (intercalate, sortBy)
import Data.Maybe (listToMaybe)
import Control.Monad.Trans.Either (EitherT, runEitherT, hoistEither)
import Control.Error (throwT)
import Control.Monad.Trans.Writer.Strict
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)


import Types.Types
import TagParsers (parsedTags)

type EIO =  (EitherT String (WriterT String IO))

reverseSort :: Ord a => [a] -> [a]
reverseSort = sortBy $ flip compare

main :: IO ()
main = do
  ((), log) <- runWriterT runStuff
  putStr $ "Log: " ++ log

  where
    runStuff :: WriterT String IO ()
    runStuff = do
      eitherResult <- runEitherT release
      case eitherResult of
        Right result -> return ()
        Left err -> do
          tell err
          return ()

    release :: EIO ()
    release = do
      (releaseNickname:_) <- liftIO getArgs

      fetchTags
      log "Fetched tags"

      cutReleaseBranch releaseNickname
      log $ "Cut release branch, " ++ releaseNickname

      releaseCandidateTag <- tagReleaseCandidate
      git ["push", "origin", releaseNickname, "--tags"]

      log "Deploying to preproduction"
      deployTag releaseCandidateTag


      log $ "Release candidate " ++ show(releaseCandidateTag) ++ "/" ++ releaseNickname ++ " has been deployed. Evaluate this release on http://preprod.gust.com."

      return()


      where
        log message = lift $ tell $ message ++ "\n"

        deployTag :: Tag -> EIO String
        deployTag tag = do
          executeExternal "DEPLOY_MIGRATIONS=true rake" ["preproduction", "deploy:force[" ++ show(tag) ++ "]"]

        tagReleaseCandidate :: EIO Tag
        tagReleaseCandidate = do
          tagString <- git ["tag"]
          tag <- hoistEither $ getNextReleaseCandidateTag <$> getLatestTag releaseTag tagString
          git ["tag", show tag]
          return tag


        cutReleaseBranch :: String -> EIO ()
        cutReleaseBranch releaseNickname = do
          tag <- hoistEither =<< getLatestTag ciTag <$> (git ["tag"])
          git ["checkout", "-b", releaseNickname, (show tag)]
          return ()

        fetchTags :: EIO ()
        fetchTags = git ["fetch", "--tags"] >> return ()

        getLatestTag :: (Tag -> Bool) -> String -> Either String Tag
        getLatestTag tagsFilter tagString = (head . reverseSort . filter tagsFilter) <$> parsedTags tagString

        getNextReleaseCandidateTag :: Tag -> Tag
        getNextReleaseCandidateTag tag =
          ReleaseCandidateTag nextRelease 1
          where
            latestRelease = version tag
            nextRelease = nextMinorVersion latestRelease

            nextMinorVersion :: Version -> Version
            nextMinorVersion (SemVer major minor patch) = SemVer major (minor + 1) patch

        releaseTag :: Tag -> Bool
        releaseTag (ReleaseTag _) = True
        releaseTag _ = False

        ciTag :: Tag -> Bool
        ciTag (CiTag _) = True
        ciTag _ = False

        git :: [String] -> EIO String
        git args = do
          executeExternal "git" args


        executeExternal :: String -> [String] -> EIO String
        executeExternal cmd args = do
          result <- liftIO $ readProcessWithExitCode cmd args ""
          case result of
            (ExitSuccess, stdout, _)  -> return stdout
            (ExitFailure _, _, err)   -> fail $ "Error: " ++ err


