module Language.NLambda.Interpreter
  ( Step(..)
  , InterpretError(..)
  , steps
  , runLambda
  ) where

import Language.NLambda
import Language.NLambda.Reductor

import Data.List (unfoldr)

data Step = Step Lambda | Result Lambda | Loop Lambda | Error InterpretError deriving (Show, Eq)
data InterpretError = Complicated deriving (Show, Eq)

-- returns the beta-reduction sequence
steps :: Int -> Lambda -> [Step]
steps maxSize = unfoldr (step maxSize <$>) . Just

-- solves a lambda expression
runLambda :: Int -> Int -> Lambda -> Step
runLambda maxSteps maxSize e =
  case last $ take maxSteps $ steps maxSize e of
    Step _ -> Error Complicated
    s      -> s

----------------------------------------------------------------
-- internal utilities
----------------------------------------------------------------

step :: Int -> Lambda -> (Step, Maybe Lambda)
step maxSize e
  | size e > maxSize = (Error Complicated, Nothing)
  | otherwise =
    case reduce e of
      Just next
        | next `alphaEquiv` e -> (Loop e, Nothing)
        | otherwise -> (Step e, Just next)
      Nothing -> (Result e, Nothing)

size :: Lambda -> Int
size (Var _)       = 1
size (Lambda _ e)  = 1 + size e
size (Apply e1 e2) = size e1 + size e2
