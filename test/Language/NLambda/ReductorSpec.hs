module Language.NLambda.ReductorSpec
  ( main
  , spec
  ) where

import Test.Hspec

import Language.NLambda
import Language.NLambda.Reductor

import Control.Monad (forM_)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "reduce" $ do
    forM_
      [ (Lambda "x" (Var "x") >-> Var "x", Var "x")  -- (\x. x) x -> x
      , (Lambda "x" (Var "y") >-> Var "x", Var "y")  -- (\x. y) x -> y
      , (Lambda "x" (Var "y") >-> Var "z", Var "y")  -- (\x. y) z -> y
      , (Lambda "x" (Lambda "x" (Var "x")) >-> Var "x", Lambda "x" (Var "x"))  -- (\x. \x. x) x -> \x. x
      , (Lambda "y" (Lambda "x" (Var "x")) >-> Var "x", Lambda "x'" (Var "x'"))  -- (\y. \x. x) x -> \x'. x'
      , (Lambda "x" (Lambda "y" (Var "x")) >-> Var "x", Lambda "y" (Var "x"))  -- (\x. \y. x) x -> \y. x
      , (Var "x" >-> (Lambda "y" (Var "y") >-> Var "z"), Var "x" >-> Var "z")  -- x ((\y. y) z) -> x z
      , (Lambda "x" (Lambda "y" (Var "y") >-> Var "z"), Lambda "x" (Var "z"))  -- \x. (\y. y) z -> \x. z
      , (Lambda "x" (Lambda "x" (Lambda "x" (Var "x"))) >-> Var "x", Lambda "x" (Lambda "x" (Var "x")))  -- (\x. \x. \x. x) x -> \x. \x. x
      , (Lambda "x" (Lambda "x" (Lambda "x" (Var "x"))) >-> Var "y", Lambda "x" (Lambda "x" (Var "x")))  -- (\x. \x. \x. x) y -> \x. \x. x
      , (Lambda "y" (Lambda "x" (Lambda "x" (Var "x"))) >-> Var "x", Lambda "x'" (Lambda "x'" (Var "x'")))  -- (\y. \x. \x. x) x -> \x'. \x'. x'
      , (Lambda "x" (Lambda "y" (Lambda "x" (Var "x"))) >-> Var "x", Lambda "y" (Lambda "x" (Var "x")))  -- (\x. \y. \x. x) x -> \y. \x. x
      , (Lambda "x" (Lambda "x" (Lambda "y" (Var "x"))) >-> Var "x", Lambda "x" (Lambda "y" (Var "x")))  -- (\x. \x. \y. x) x -> \x. \y. x
      , (Lambda "x" (Lambda "y" (Var "x")) >-> Lambda "y" (Var "y"), Lambda "y" (Lambda "y" (Var "y")))  -- (\x y. x) (\y. y) -> \y. \y. y
      , (Lambda "y" (Lambda "x" (Var "x" >-> Var "y")) >-> Var "x", Lambda "x'" (Var "x'" >-> Var "x"))  -- (\y. \x. x y) x -> \x'. x' x
      , (Lambda "y" (Lambda "x" (Var "x'" >-> Var "y")) >-> Var "x", Lambda "x''" (Var "x'" >-> Var "x"))  -- (\y. \x. x' y) x -> \x''. x' x
      , (Lambda "y" (Lambda "x'" (Var "x'" >-> Var "y")) >-> Var "x", Lambda "x'" (Var "x'" >-> Var "x"))  -- (\y. \x'. x' y) x -> \x'. x' x
      , (Lambda "x'" (Lambda "x" (Var "x''" >-> Var "x'")) >-> Var "x", Lambda "x'''" (Var "x''" >-> Var "x"))  -- (\x'. \x. x'' x') x -> \x'''. x'' x
      , (Lambda "x" (Var "x" >-> Var "x") >-> Lambda "x" (Var "x" >-> Var "x"), Lambda "x" (Var "x" >-> Var "x") >-> Lambda "x" (Var "x" >-> Var "x"))  -- (\x. x x) (\x. x x) -> (\x. x x) (\x. x x)
      ] $ \(expression, result) ->
        it ("reduces " ++ show expression) $ reduce expression `shouldBe` Just result

    forM_
      [ Var "x"  -- x
      , Lambda "x" (Var "x")  -- \x. x
      , Lambda "x" (Var "y")  -- \x. y
      , Var "x" >-> Var "x"  -- x x
      ] $ \expression ->
        it ("can't reduce " ++ show expression) $ reduce expression `shouldBe` Nothing


  describe "alphaEquiv" $ do
    forM_
      [ (Var "x", Var "x")  -- x = x
      , (Lambda "x" (Var "x"), Lambda "x" (Var "x"))  -- \x. x = \x. x
      , (Lambda "x" (Var "x"), Lambda "y" (Var "y"))  -- \x. x = \y. y
      , (Lambda "x" (Var "y"), Lambda "z" (Var "y"))  -- \x. y = \z. y
      , (Lambda "x" (Lambda "x" (Var "x")), Lambda "y" (Lambda "z" (Var "z")))  -- \x. \x. x = \y. \z. z
      , (Lambda "y" (Lambda "z" (Var "z")), Lambda "x" (Lambda "x" (Var "x")))  -- \y. \z. z = \x. \x. x
      , (Lambda "a" (Lambda "b" (Var "b")), Lambda "c" (Lambda "d" (Var "d")))  -- \a. \b. b = \c. \d. d
      , (Lambda "x" (Var "x" >-> Var "x"), Lambda "y" (Var "y" >-> Var "y"))  -- \x. x x = \y. y y
      ] $ \(e1, e2) ->
        it ("returns True when given alpha-equivalant expressions " ++ show e1 ++ " and " ++ show e2) $ alphaEquiv e1 e2 `shouldBe` True

    forM_
      [ (Var "x", Var "y")  -- x /= y
      , (Lambda "x" (Var "x"), Lambda "x" (Var "y"))  -- \x. x /= \x. y
      , (Lambda "x" (Var "y"), Lambda "x" (Var "x"))  -- \x. y /= \x. x
      , (Lambda "x" (Var "x"), Lambda "y" (Var "x"))  -- \x. x /= \y. x
      , (Lambda "y" (Var "x"), Lambda "x" (Var "x"))  -- \y. x /= \x. x
      , (Lambda "x" (Lambda "x" (Var "y")), Lambda "y" (Lambda "z" (Var "y")))  -- \x. \x. y /= \y. \z. y
      , (Lambda "y" (Lambda "z" (Var "y")), Lambda "x" (Lambda "x" (Var "y")))  -- \y. \z. y /= \x. \x. y
      , (Lambda "x" (Lambda "x" (Var "y")), Lambda "z" (Lambda "y" (Var "y")))  -- \x. \x. y /= \z. \y. y
      , (Lambda "z" (Lambda "y" (Var "y")), Lambda "x" (Lambda "x" (Var "y")))  -- \z. \y. y /= \x. \x. y
      , (Var "x", Lambda "y" (Var "x"))  -- x /= \y. x
      , (Lambda "y" (Var "x"), Var "x")  -- \y. x /= x
      ] $ \(e1, e2) ->
        it ("returns False when given non-alpha-equivalant expressions " ++ show e1 ++ " and " ++ show e2) $ alphaEquiv e1 e2 `shouldBe` False
