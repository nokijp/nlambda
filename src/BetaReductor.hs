module BetaReductor
  ( Lambda(..)
  , reduct
  ) where

import Control.Applicative
import qualified Data.List as L
import qualified Data.Set as S

data Lambda = Var String | Lambda String Lambda | Apply Lambda Lambda deriving (Eq, Show)

freeVariables :: Lambda -> S.Set String
freeVariables (Var s) = S.singleton s
freeVariables (Lambda s e) = S.delete s $ freeVariables e
freeVariables (Apply e1 e2) = freeVariables2 e1 e2

freeVariables2 :: Lambda -> Lambda -> S.Set String
freeVariables2 e1 e2 = freeVariables e1 `S.union` freeVariables e2

newVariable :: S.Set String -> String -> String
newVariable useds base = let candidates = iterate (++ "'") base
                         in (\(Just x) -> x) $ L.find (`S.notMember` useds) candidates

apply :: Lambda -> String -> Lambda -> Lambda
apply e s1 (Var s2) | s1 == s2  = e
                    | otherwise = Var s2
apply e1 s1 (Lambda s2 e2) = let s2' = newVariable (freeVariables2 e2 e1) s2
                             in Lambda s2' $ apply e1 s1 (apply (Var s2') s2 e2)
apply e1 s (Apply e2 e3) = Apply (apply e1 s e2) (apply e1 s e3)

reduct :: Lambda -> Maybe Lambda
reduct (Var _) = Nothing
reduct (Lambda s e) = Lambda s <$> reduct e
reduct (Apply (Lambda s e1) e2) = Just $ apply e2 s e1
reduct (Apply e1 e2) = ((`Apply` e2) <$> reduct e1) <|> (Apply e1 <$> reduct e2)
