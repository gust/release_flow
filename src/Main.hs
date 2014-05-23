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
      lift $ tell "Fetched tags\n"

      cutReleaseBranch releaseNickname
      lift $ tell $ "Cut release branch, " ++ releaseNickname ++ "\n"

      tagReleaseCandidate
      git ["push", "origin", releaseNickname, "--tags"]
      return ()


    tagReleaseCandidate :: EIO ()
    tagReleaseCandidate = do
      tagString <- git ["tag"]
      tag <- hoistEither $ getNextReleaseCandidateTag =<< getLatestTag releaseTag tagString
      git ["tag", show tag]
      return ()


    cutReleaseBranch :: String -> EIO ()
    cutReleaseBranch releaseNickname = do
      eitherTag <- fmap (getLatestTag ciTag) $ git ["tag"]
      tag <- case eitherTag of
        Right tag -> return tag
        Left err -> fail err
      git ["checkout", "-b", releaseNickname, (show tag)]
      return ()

    fetchTags :: EIO ()
    fetchTags = git ["fetch", "--tags"] >> return ()

    getLatestTag :: (Tag -> Bool) -> String -> Either String Tag
    getLatestTag tagsFilter tagString = (head . reverseSort . filter tagsFilter) <$> parsedTags tagString

    -- TODO: don't actually need Either here
    getNextReleaseCandidateTag :: Tag -> Either String Tag
    getNextReleaseCandidateTag tag =
      return $ ReleaseCandidateTag nextRelease 1
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
      result <- liftIO $ readProcessWithExitCode "git" args ""
      case result of
        (ExitSuccess, result, _)  -> return result
        (ExitFailure _, _, err)   -> fail $ "git error: " ++ err

