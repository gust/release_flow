{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative ((<$>))
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))
import Text.ParserCombinators.Parsec
import System.Environment (getArgs)
import Data.List (intercalate, sortBy)
import Data.Maybe (listToMaybe)
import Control.Monad.Error
import Control.Monad.Trans.Writer.Strict
import Control.Monad.IO.Class (liftIO)

import Types.Types
import TagParsers (tagsParser)

type EIO =  (ErrorT String (WriterT String IO))

reverseSort :: Ord a => [a] -> [a]
reverseSort = sortBy $ flip compare

main :: IO ()
main = do
  ((), log) <- runWriterT runStuff
  putStr $ "Log: " ++ log

  where

    runStuff :: WriterT String IO ()
    runStuff = do
      eitherResult <- runErrorT release
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
      tag <- hoistEIO $ getNextReleaseCandidateTag =<< getLatestReleaseTag tagString
      git ["tag", show tag]
      return ()
      where
        hoistEIO :: Either String Tag -> EIO Tag
        hoistEIO eitherTag = case eitherTag of
          Right tag -> return tag
          Left err -> fail err

    cutReleaseBranch :: String -> EIO ()
    cutReleaseBranch releaseNickname = do
      eitherTag <- fmap getLatestGreenTag $ git ["tag"]
      tag <- case eitherTag of
        Right tag -> return tag
        Left err -> fail err
      git ["checkout", "-b", releaseNickname, (show tag)]
      return ()

    fetchTags :: EIO ()
    fetchTags = git ["fetch", "--tags"] >> return ()

    getLatestGreenTag :: String -> Either String Tag
    getLatestGreenTag tagString = (head . reverseSort . filter ciTag) <$> parsedTags tagString

    getLatestReleaseTag :: String -> Either String Tag
    getLatestReleaseTag tagString = (head . reverseSort . filter releaseTag) <$> parsedTags tagString

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

    parsedTags :: String -> Either String [Tag]
    parsedTags = 
      convertToStringError . parse tagsParser ""
      where
        convertToStringError :: Either ParseError a -> Either String a
        convertToStringError (Left parseError) = Left $ show parseError
        convertToStringError (Right x) = Right x

    git :: [String] -> EIO String
    git args = do
      result <- liftIO $ readProcessWithExitCode "git" args ""
      case result of
        (ExitSuccess, result, _)  -> return result
        (ExitFailure _, _, err)   -> fail $ "git error: " ++ err

