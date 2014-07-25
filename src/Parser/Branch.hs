module Parser.Branch where

import Types                               (Branch(..))
import Text.ParserCombinators.Parsec       (Parser, many, manyTill, parse, noneOf, char)
import Text.ParserCombinators.Parsec.Error (ParseError, errorMessages,
                                            messageString)
import Data.List                           (intercalate)
import Control.Applicative ((<$>))

parsedBranches :: String -> Either String [Branch]
parsedBranches = convertToStringError . parse branchParser ""
  where
    convertToStringError :: Either ParseError a -> Either String a
    convertToStringError (Left parseError) = Left $ intercalate ", " $ map messageString $ errorMessages parseError
    convertToStringError (Right x) = Right x

branchParser :: Parser [Branch]
branchParser = many $ Branch <$> tillEol

tillEol = manyTill (noneOf "\n") eol
eol = char '\n'
