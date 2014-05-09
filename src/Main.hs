{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative ((<$>))
import System.Process (readProcess)
import Text.ParserCombinators.Parsec
import System.Environment (getArgs)
import Data.List (intercalate, sortBy)
import Data.Maybe (listToMaybe)

import Types.Types
import TagParsers (tagsParser)

{- data Branch = LocalBranch{ -}
    {- bName :: String -}
  {- } | RemoteBranch{ -}
    {- bName       :: String -}
  {- , originName  :: String -}
  {- } deriving (Show) -}

reverseSort :: Ord a => [a] -> [a]
reverseSort = sortBy $ flip compare

handleError :: Show a => a -> IO ()
handleError = putStr . show

main :: IO ()
main = do
  (releaseNickname:_) <- getArgs

  fetchTags
  cutReleaseBranch releaseNickname
  tagReleaseCandidate

  where
    tagReleaseCandidate :: IO ()
    tagReleaseCandidate = do
      latestReleaseTag <- getLatestReleaseTag
      case latestReleaseTag of
        Left err -> handleError err
        Right tag -> do
          let latestRelease = version tag
          let nextRelease = nextMinorVersion latestRelease
          let tag = ReleaseCandidateTag nextRelease 1
          git ["tag", show tag]
          return ()

      where
        nextMinorVersion :: Version -> Version
        nextMinorVersion (SemVer major minor patch) = SemVer major (minor + 1) patch

    cutReleaseBranch :: String -> IO ()
    cutReleaseBranch releaseNickname = do
      tag <- getLatestGreenTag
      case tag of
        Left err -> handleError err
        Right tag -> do
          putStr $ "tag: " ++ (show tag)
          git ["checkout", "-b", releaseNickname, (show tag)]
          return ()

    fetchTags :: IO ()
    fetchTags = git ["fetch", "--tags"] >> return ()

    getLatestGreenTag :: IO (Either ParseError Tag)
    getLatestGreenTag = do
      tagString <- git ["tag"]
      return $ (head . reverseSort . filter ciTag) <$> parsedTags tagString

    getLatestReleaseTag :: IO (Either ParseError Tag)
    getLatestReleaseTag = do
      tagString <- git ["tag"]
      return $ (head . reverseSort . filter releaseTag) <$> parsedTags tagString

    releaseTag :: Tag -> Bool
    releaseTag (ReleaseTag _) = True
    releaseTag _ = False

    ciTag :: Tag -> Bool
    ciTag (CiTag _) = True
    ciTag _ = False

    parsedTags :: String -> Either ParseError [Tag]
    parsedTags = parse tagsParser ""

    git :: [String] -> IO String
    git args = readProcess "git" args ""


  {- originalBranch <- currentBranch -}
  {- branch <- createBranch $ LocalBranch "temp_branch" -}


  {- branches <- listAllBranches -}
  {- putStr $ "all branches: " ++ (show branches) -}

  {- checkout originalBranch -}
  {- destroyBranch branch -}

    {- where -}

      {- currentBranch :: IO Branch -}
      {- currentBranch = (head . parsedBranches) <$> git ["rev-parse", "--abbrev-ref", "HEAD"] -}
      
      {- createBranch :: Branch -> IO Branch -}
      {- createBranch branch@(LocalBranch name) = do -}
        {- git ["checkout", "-b", name] -}
        {- return branch -}

      {- destroyBranch :: Branch -> IO () -}
      {- destroyBranch (LocalBranch name) = do -}
        {- git ["branch", "-D", name] -}
        {- return () -}

      {- listAllBranches :: IO ([Branch]) -}
      {- listAllBranches = parsedBranches <$> git ["branch", "-a"] -}

      {- checkout :: Branch -> IO String -}
      {- checkout (LocalBranch name) = git ["checkout", name] -}

      {- git :: [String] -> IO String -}
      {- git args = readProcess "git" args "" -}


      {- parsedBranches :: String -> [Branch] -}
      {- parsedBranches s = case parse parseBranches "" s of -}
        {- Right branches -> branches -}
        {- Left err -> [] -}

{- [> parseTs = many1 $ parseT <] -}

{- [> parseT = do <] -}
  {- [> t <- manyTill (noneOf "\n") $ eol <] -}
  {- [> return t <] -}

        {- where -}
          {- parseBranches :: Parser [Branch] -}
          {- parseBranches = many1 $ (try activeLocalBranch) -}
                                    {- <|> (try remoteBranch) -}
                                    {- <|> localBranch -}

          {- activeLocalBranch :: Parser Branch -}
          {- activeLocalBranch = do -}
            {- char '*' -}
            {- spaces -}
            {- branchName <- manyTill (noneOf "\n") eol -}
            {- return $ LocalBranch branchName -}

          {- remoteBranch = do -}
            {- spaces -}
            {- string "remotes/" -}
            {- originName <- manyTill (noneOf "/") $ char '/' -}
            {- branchName <- manyTill (noneOf "\n") $ eol -}
            {- return $ RemoteBranch branchName originName -}

          {- localBranch = do -}
            {- spaces -}
            {- branchName <- manyTill (noneOf "\n") $ eol -}
            {- return $ LocalBranch branchName -}

          {- eol = char '\n' -}


