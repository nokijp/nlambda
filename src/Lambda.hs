module Lambda
 ( Lambda(..)
 ) where

data Lambda = Var String | Lambda String Lambda | Apply Lambda Lambda deriving (Eq, Show)

