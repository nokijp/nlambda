module Language.NLambda.SKI
  ( SKI(..)
  , (<>)
  , skiString
  , skiToLambda
  , lambdaToSki
  , freeVariables
  ) where

import qualified Language.NLambda as L
import qualified Language.NLambda.Reductor as LR

import qualified Data.Set as S

data SKI = S | K | I | Var !String | Apply SKI SKI deriving (Eq, Show)

(<>) :: SKI -> SKI -> SKI
(<>) = Apply

-- converts a SKI into a readable string
skiString :: SKI -> String
skiString (Var s) = s
skiString (Apply e1 e2) = skiString e1 ++ " " ++ quote e2
  where
    quote e@(Apply _ _) = "(" ++ skiString e ++ ")"
    quote e = skiString e
skiString S = "S"
skiString K = "K"
skiString I = "I"

-- converts a SKI into a Lambda
skiToLambda :: SKI -> L.Lambda
skiToLambda S = L.Lambda "x" $ L.Lambda "y" $ L.Lambda "z" $ L.Var "x" L.<> L.Var "z" L.<> (L.Var "y" L.<> L.Var "z")
skiToLambda K = L.Lambda "x" $ L.Lambda "y" $ L.Var "x"
skiToLambda I = L.Lambda "x" $ L.Var "x"
skiToLambda (Var s) = L.Var s
skiToLambda (Apply e1 e2) = skiToLambda e1 L.<> skiToLambda e2

-- converts a Lambda into a SKI by the T[]-transformation
lambdaToSki :: L.Lambda -> SKI
lambdaToSki (L.Var s) = Var s
lambdaToSki (L.Apply e1 e2) = lambdaToSki e1 <> lambdaToSki e2
lambdaToSki (L.Lambda s1 (L.Var s2))
  | s1 == s2 = I
  | otherwise = K <> Var s2
lambdaToSki (L.Lambda s1 a@(L.Apply e v@(L.Var s2)))
  | s1 `S.member` LR.freeVariables e = S <> lambdaToSki (L.Lambda s1 e) <> lambdaToSki (L.Lambda s1 v)
  | s1 == s2 = lambdaToSki e
  | otherwise = K <> lambdaToSki a
lambdaToSki (L.Lambda s a@(L.Apply e1 e2))
  | s `S.notMember` LR.freeVariables a = K <> lambdaToSki a
  | otherwise = S <> lambdaToSki (L.Lambda s e1) <> lambdaToSki (L.Lambda s e2)
lambdaToSki (L.Lambda s1 l@(L.Lambda s2 e))
  | s1 `S.notMember` LR.freeVariables l = K <> lambdaToSki l
  | otherwise = skiToSki s1 (lambdaToSki $ L.Lambda s2 e)

-- returns free variables in an expression
freeVariables :: SKI -> S.Set String
freeVariables (Var s) = S.singleton s
freeVariables (Apply e1 e2) = freeVariables e1 `S.union` freeVariables e2
freeVariables _ = S.empty

----------------------------------------------------------------
-- internal utilities
----------------------------------------------------------------

skiToSki :: String -> SKI -> SKI
skiToSki s1 v@(Var s2)
  | s1 == s2 = I
  | otherwise = K <> v
skiToSki s1 a@(Apply e v@(Var s2))
  | s1 `S.member` freeVariables e = S <> skiToSki s1 e <> skiToSki s1 v
  | s1 == s2 = e
  | otherwise = K <> a
skiToSki s a@(Apply e1 e2)
  | s `S.notMember` freeVariables a = K <> a
  | otherwise = S <> skiToSki s e1 <> skiToSki s e2
skiToSki _ ski = K <> ski
