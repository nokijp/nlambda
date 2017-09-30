module Language.NLambda.Reductor
  ( alphaEquiv
  , reduce
  , freeVariables
  ) where

import Language.NLambda

import Control.Applicative
import qualified Data.List as L
import qualified Data.Set as S

-- reduces a lambda expression step-by-step
reduce :: Lambda -> Maybe Lambda
reduce (Var _)                  = Nothing
reduce (Lambda s e)             = Lambda s <$> reduce e
reduce (Apply (Lambda s e1) e2) = Just $ apply e1 s e2
reduce (Apply e1 e2)            = ((>-> e2) <$> reduce e1) <|> ((e1 >->) <$> reduce e2)

-- evaluates alpha-equivalence between two lambda expressions
alphaEquiv :: Lambda -> Lambda -> Bool
alphaEquiv (Var s1)          (Var s2)          = s1 == s2
alphaEquiv (Apply e1 e2)     (Apply e3 e4)     = (e1 `alphaEquiv` e3) && (e2 `alphaEquiv` e4)
alphaEquiv l1@(Lambda s1 e1) l2@(Lambda s2 e2) = (freeVariables l1 == freeVariables l2) && (e1 `alphaEquiv` apply e2 s2 (Var s1))
alphaEquiv _                 _                 = False

-- returns free variables in an expression
freeVariables :: Lambda -> S.Set String
freeVariables (Var s)       = S.singleton s
freeVariables (Lambda s e)  = S.delete s $ freeVariables e
freeVariables (Apply e1 e2) = freeVariables e1 `S.union` freeVariables e2

----------------------------------------------------------------
-- internal utilities
----------------------------------------------------------------

-- generates a new variable name which is not duplicated with used ones
newVariable :: S.Set String -> String -> String
newVariable useds base =
  let candidates = iterate (++ "'") base
  in (\(Just x) -> x) $ L.find (`S.notMember` useds) candidates

-- substitutes the third argument for variables with the same name as the second argument in the first argument
apply :: Lambda -> String -> Lambda -> Lambda
apply (Var s1) s2 e
  | s1 == s2  = e
  | otherwise = Var s1
apply l@(Lambda s1 e1) s2 e2
  | s1 == s2  = l
  | otherwise =
    let s1' = newVariable (S.delete s1 (freeVariables e1) `S.union` freeVariables e2) s1
    in Lambda s1' $ apply (apply e1 s1 (Var s1')) s2 e2
apply (Apply e1 e2) s e3 = apply e1 s e3 >-> apply e2 s e3
