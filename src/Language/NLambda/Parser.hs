module Language.NLambda.Parser
  ( lambdaParser
  ) where

import Language.NLambda

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Token
import Text.Parsec.Language

-- | parses a lambda expression
lambdaParser :: Parser Lambda
lambdaParser = spaces *> expression <* spaces

----------------------------------------------------------------
-- internal parsers
----------------------------------------------------------------

expression :: Parser Lambda
expression = nestedApply <$> many1 (block <|> lambda <|> variable <|> letIn) <?> "expression"
  where
    nestedApply :: [Lambda] -> Lambda
    nestedApply [] = error "impossible to reach here"
    nestedApply (e:es) = foldl Apply e es

block :: Parser Lambda
block = parens tokenParser expression

variableName :: Parser String
variableName = identifier tokenParser <?> "variable name"

variable :: Parser Lambda
variable = Var <$> variableName <?> "variable"

lambda :: Parser Lambda
lambda = nestedLambda <$> (lambdaSign *> arguments <* argumentsTerminator) <*> expression
  where
    nestedLambda args e = foldr Lambda e args
    lambdaSign = reservedOp tokenParser "\\" <|> reservedOp tokenParser "λ"
    arguments = many1 variableName <?> "arguments"
    argumentsTerminator = reservedOp tokenParser "." <|> reservedOp tokenParser "->"

letIn :: Parser Lambda
letIn = applyLetIns <$> (reserved tokenParser "let" *> definitions <* reserved tokenParser "in") <*> expression
  where
    applyLetIns defs e = foldr (\(v, ve) ee -> Lambda v ee >-> ve) e defs
    definitions = definition `sepBy1` reservedOp tokenParser ","
    definition = (,) <$> (variableName <* reservedOp tokenParser "=") <*> expression

tokenParser :: TokenParser st
tokenParser = makeTokenParser def
  where
    def = emptyDef { commentLine = "#"
                   , opStart = opLetter def
                   , opLetter = oneOf "\\λ.->=,"
                   , reservedOpNames= ["\\", "λ", ".", "->", "=", ","]
                   , reservedNames = ["let", "in"]
                   }
