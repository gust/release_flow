{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative ((<$>))
import System.Process (readProcess)
import Text.ParserCombinators.Parsec
import Data.Maybe (catMaybes)

{- data Branch = LocalBranch{ -}
    {- bName :: String -}
  {- } | RemoteBranch{ -}
    {- bName       :: String -}
  {- , originName  :: String -}
  {- } deriving (Show) -}

data Version = SemVer {
    major :: Int
  , minor :: Int
  , patch :: Int
  } deriving (Show)

data Tag = ReleaseTag {
    version :: Version
  } deriving (Show)

main :: IO ()
main = do
  fetchTags
  tag <- getLatestTag
  case tag of
    Left err -> putStr $ show err
    Right tag -> putStr $ "tag: " ++ (show tag)
  return ()

  where
    fetchTags :: IO ()
    fetchTags = git ["fetch", "--tags"] >> return ()

    getLatestTag :: IO (Either ParseError Tag)
    getLatestTag = do

      tagString <- git ["tag"]
      return $ head <$> parsedTags tagString

      where
        parsedTags :: String -> Either ParseError [Tag]
        parsedTags = parse tagsParser ""

          where
            tagsParser :: Parser [Tag]
            tagsParser = catMaybes <$> (many $ releaseTagParser <|> crapParser)

            crapParser :: Parser (Maybe Tag)
            crapParser = tillEol >> return Nothing

            releaseTagParser :: Parser (Maybe Tag)
            releaseTagParser = do
              string "release"
              char '/'
              tag <- ReleaseTag <$> semVerParser
              eol
              return $ Just tag

            semVerParser :: Parser Version
            semVerParser = do
              major <- intParser
              char '.'
              minor <- intParser
              char '.'
              patch <- intParser
              return $ SemVer major minor patch
              where
                intParser :: Parser Int
                intParser = read <$> many1 digit

            tillEol = manyTill (noneOf "\n") eol
            eol = char '\n'

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


