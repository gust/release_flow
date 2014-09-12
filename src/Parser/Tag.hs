{-# LANGUAGE OverloadedStrings #-}

module Parser.Tag
  (
    parsedTags
  )
  where

import           Control.Applicative                 ((<$>), (<*), (<*>))
import           Data.List                           (intercalate)
import           Data.Maybe                          (catMaybes)
import           Text.ParserCombinators.Parsec       (Parser, char, digit, many,
                                                      many1, manyTill, noneOf,
                                                      parse, string, try, (<|>))
import           Text.ParserCombinators.Parsec.Error (ParseError, errorMessages,
                                                      messageString)

import           Types


parsedTags :: String -> [Tag]
parsedTags =
  handleError . parse tagsParser ""
  where
    handleError :: Either ParseError a -> a
    handleError (Left parseError) = error $ show parseError
    handleError (Right x) = x

tagsParser :: Parser [Tag]
tagsParser = catMaybes <$> (many $
      (try releaseCandidateTagParser)
  <|> (try releaseTagParser)
  <|> ciTagParser
  <|> crapParser)


releaseCandidateTagParser :: Parser (Maybe Tag)
releaseCandidateTagParser = do
  string releaseTagPrefix
  tag <- ReleaseCandidateTag <$> semVerParser <*> releaseCandidateParser
  eol
  return $ Just tag


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

releaseCandidateParser :: Parser Int
releaseCandidateParser = do
  string "-rc"
  intParser

intParser :: Parser Int
intParser = read <$> many1 digit

crapParser :: Parser (Maybe Tag)
crapParser = tillEol >> return Nothing

tillEol = manyTill (noneOf "\n") eol
eol = char '\n'


