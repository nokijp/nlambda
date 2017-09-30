module Language.NLambda
  ( Lambda(..)
  , (<>)
  , lambdaString
  ) where

data Lambda = Var !String | Lambda !String Lambda | Apply Lambda Lambda deriving (Eq, Show)

(<>) :: Lambda -> Lambda -> Lambda
(<>) = Apply

-- converts a Lambda into a readable string
lambdaString :: Lambda -> String
lambdaString (Var s)       = s
lambdaString (Lambda s e)  = flatLambdaString [s] e
lambdaString (Apply e1 e2) = quoteLambda e1 ++ " " ++ quoteLambdaApply e2

----------------------------------------------------------------
-- internal utilities
----------------------------------------------------------------

flatLambdaString :: [String] -> Lambda -> String
flatLambdaString ss (Lambda s e) = flatLambdaString (s:ss) e
flatLambdaString ss e            = "\\" ++ unwords (reverse ss) ++ ". " ++ lambdaString e

quoteLambda :: Lambda -> String
quoteLambda e@(Lambda _ _) = quote $ lambdaString e
quoteLambda e              = lambdaString e

quoteLambdaApply :: Lambda -> String
quoteLambdaApply e@(Var _) = lambdaString e
quoteLambdaApply e         = quote $ lambdaString e

quote :: String -> String
quote s = "(" ++ s ++ ")"
