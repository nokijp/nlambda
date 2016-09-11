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

apply :: String -> Lambda -> Lambda -> Lambda
apply s1 (Var s2) e | s1 == s2  = e
                    | otherwise = Var s2
apply s1 (Lambda s2 e1) e2 = let s2' = newVariable (freeVariables2 e1 e2) s2
                             in Lambda s2' $ apply s1 (apply s2 e1 (Var s2')) e2
apply s (Apply e1 e2) e3 = Apply (apply s e1 e3) (apply s e2 e3)

reduct :: Lambda -> Maybe Lambda
reduct (Var _) = Nothing
reduct (Lambda s e) = Lambda s <$> reduct e
reduct (Apply (Lambda s e1) e2) = Just $ apply s e1 e2
reduct (Apply e1 e2) = ((`Apply` e2) <$> reduct e1) <|> (Apply e1 <$> reduct e2)
