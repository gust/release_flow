module Parser.Branch where

import Types                               (Branch(..))
import Text.ParserCombinators.Parsec       (Parser, many, manyTill, parse, noneOf, char, optional, spaces)
import Text.ParserCombinators.Parsec.Error (ParseError, errorMessages,
                                            messageString)
import Data.List                           (intercalate)
import Control.Applicative ((<$>))

parsedBranches :: String -> [Branch]
parsedBranches = handleError . parse branchParser ""
  where
    handleError :: Either ParseError a -> a
    handleError (Left parseError) = error $ show parseError
    handleError (Right x) = x

branchParser :: Parser [Branch]
branchParser = many $ ((optional $ char '*') >> spaces >> (Branch <$> tillEol))

tillEol = manyTill (noneOf "\n") eol
eol = char '\n'
