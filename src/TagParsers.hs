{-# LANGUAGE OverloadedStrings #-}

module TagParsers 
  (
    tagsParser
  )
  where

import Control.Applicative ((<$>), (<*>), (<*))
import Data.Maybe (catMaybes)
import Text.ParserCombinators.Parsec

import Types.Types

tagsParser :: Parser [Tag]
tagsParser = catMaybes <$> (many $ (try releaseTagParser) <|> ciTagParser <|> crapParser)

releaseTagParser :: Parser (Maybe Tag)
releaseTagParser = do
  string releaseTagPrefix
  tag <- ReleaseTag <$> semVerParser
  eol
  return $ Just tag

ciTagParser :: Parser (Maybe Tag)
ciTagParser = do
  string ciTagPrefix
  tag <- CiTag . UnixTimeVer <$> intParser
  eol
  return $ Just tag

semVerParser :: Parser Version
semVerParser = SemVer <$> intParser <* (char '.') 
                      <*> intParser <* (char '.') 
                      <*> intParser

  {- major <- intParser -}
  {- char '.' -}
  {- minor <- intParser -}
  {- char '.' -}
  {- patch <- intParser -}
  {- return $ SemVer major minor patch -}

intParser :: Parser Int
intParser = read <$> many1 digit

crapParser :: Parser (Maybe Tag)
crapParser = tillEol >> return Nothing

tillEol = manyTill (noneOf "\n") eol
eol = char '\n'


