module Language.NLambdaSpec
  ( main
  , spec
  ) where

import Test.Hspec

import Language.NLambda

import Control.Monad

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "(<>)" $ do
    forM_
      [ (Var "a" <> Var "b", Apply (Var "a") (Var "b"))
      , (Var "a" <> Var "b" <> Var "c", Apply (Apply (Var "a") (Var "b")) (Var "c"))
      , (Var "a" <> (Var "b" <> Var "c"), Apply (Var "a") (Apply (Var "b") (Var "c")))
      ] $ \(e1, e2) ->
        it ("is left-associative and returns " ++ show e2) $ e1 `shouldBe` e2

  describe "lambdaString" $ do
    forM_
      [ (Var "x", "x")
      , (Lambda "x" (Var "y"), "\\x. y")
      , (Var "x" <> Var "y", "x y")
      , (Lambda "x" (Lambda "y" (Var "z")), "\\x y. z")
      , (Lambda "a" (Lambda "b" (Lambda "c" (Var "d"))), "\\a b c. d")
      , (Var "a" <> Var "b" <> Var "c", "a b c")
      , (Var "a" <> Var "b" <> Var "c" <> Var "d", "a b c d")
      , (Var "a" <> (Var "b" <> Var "c") <> Var "d", "a (b c) d")
      , (Var "a" <> (Var "b" <> Var "c"), "a (b c)")
      , (Lambda "a" (Var "b") <> Var "c", "(\\a. b) c")
      , (Var "a" <> Lambda "b" (Var "c"), "a (\\b. c)")
      , (Lambda "a" (Var "b" <> Var "c"), "\\a. b c")
      ] $ \(e, s) ->
        it ("converts " ++ show e ++ " to " ++ s) $ lambdaString e `shouldBe` s
