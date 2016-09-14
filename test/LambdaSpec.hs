module LambdaSpec
  ( main
  , spec
  ) where

import Test.Hspec

import Lambda

import Control.Monad

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "lambdaString" $ do
    forM_ [ (Var "x", "x")
          , (Lambda "x" (Var "y"), "\\x. y")
          , (Apply (Var "x") (Var "y"), "x y")
          , (Lambda "x" (Lambda "y" (Var "z")), "\\x y. z")
          , (Lambda "a" (Lambda "b" (Lambda "c" (Var "d"))), "\\a b c. d")
          , (Apply (Apply (Var "a") (Var "b")) (Var "c"), "a b c")
          , (Apply (Apply (Apply (Var "a") (Var "b")) (Var "c")) (Var "d"), "a b c d")
          , (Apply (Apply (Var "a") (Apply (Var "b") (Var "c"))) (Var "d"), "a (b c) d")
          , (Apply (Var "a") (Apply (Var "b") (Var "c")), "a (b c)")
          , (Apply (Lambda "a" (Var "b")) (Var "c"), "(\\a. b) c")
          , (Apply (Var "a") (Lambda "b" (Var "c")), "a (\\b. c)")
          , (Lambda "a" (Apply (Var "b") (Var "c")), "\\a. b c")
          ] $ \(e, s) -> it ("converts " ++ show e ++ " to " ++ s) $ lambdaString e `shouldBe` s
