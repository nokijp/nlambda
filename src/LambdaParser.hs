module LambdaParser
  ( lambdaParser
  ) where

import BetaReductor
import Data.Char
import Text.Parsec
import Text.Parsec.Prim
import Text.Parsec.Char
import Text.Parsec.String

-- parses a lambda expression
lambdaParser :: Parser Lambda
lambdaParser = expression

----------------------------------------------------------------
-- internal parsers
----------------------------------------------------------------

expression :: Parser Lambda
expression = nestedApply <$> (spaces *> many1 ((block <|> lambda <|> variable) <* spaces)) <?> "expression"

block :: Parser Lambda
block = string "(" *> expression <* string ")"

variableName :: Parser String
variableName = many1 (satisfy (\c -> (not . isAscii) c || isAlpha c)) <?> "variable name"

variable :: Parser Lambda
variable = Var <$> variableName <?> "variable"

lambda :: Parser Lambda
lambda = nestedLambda <$> (lambdaSign  *> spaces *> arguments <* argumentsTerminator) <*> expression
       where
         lambdaSign = string "\\" <|> string "λ"
         arguments = (:) <$> variableName <* spaces <*> many (variableName <* spaces) <?> "arguments"
         argumentsTerminator = string "." <|> string "->"

----------------------------------------------------------------
-- internal utilities
----------------------------------------------------------------

nestedLambda :: [String] -> Lambda -> Lambda
nestedLambda args e = foldr Lambda e args

nestedApply :: [Lambda] -> Lambda
nestedApply (e:es) = foldl Apply e es
