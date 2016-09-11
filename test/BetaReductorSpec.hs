module BetaReductorSpec
  ( main
  , spec
  ) where

import Test.Hspec

import BetaReductor
import Control.Monad

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "reduct" $ do
    forM_ [ (Apply (Lambda "x" (Var "x")) (Var "x"), Var "x")  -- (\x. x) x -> x
          , (Apply (Lambda "x" (Var "y")) (Var "x"), Var "y")  -- (\x. y) x -> y
          , (Apply (Lambda "x" (Var "y")) (Var "z"), Var "y")  -- (\x. y) z -> y
          , (Apply (Lambda "x" (Lambda "x" (Var "x"))) (Var "x"), Lambda "x'" (Var "x'"))  -- (\x. \x. x) x -> \x'. x'
          , (Apply (Lambda "y" (Lambda "x" (Var "x"))) (Var "x"), Lambda "x'" (Var "x'"))  -- (\y. \x. x) x -> \x'. x'
          , (Apply (Lambda "x" (Lambda "y" (Var "x"))) (Var "x"), Lambda "y" (Var "x"))  -- (\x. \y. x) x -> \y. x
          , (Apply (Var "x") (Apply (Lambda "y" (Var "y")) (Var "z")), Apply (Var "x") (Var "z"))  -- x ((\y. y) z) -> x z
          , (Lambda "x" (Apply (Lambda "y" (Var "y")) (Var "z")), Lambda "x" (Var "z"))  -- \x. (\y. y) z -> \x. z
          , (Apply (Lambda "y" (Lambda "x" (Apply (Var "x") (Var "y")))) (Var "x"), Lambda "x'" (Apply (Var "x'") (Var "x")))  -- (\y. \x. x y) x -> \x'. x' x
          , (Apply (Lambda "y" (Lambda "x" (Apply (Var "x'") (Var "y")))) (Var "x"), Lambda "x''" (Apply (Var "x'") (Var "x")))  -- (\y. \x. x' y) x -> \x''. x' x
          , (Apply (Lambda "y" (Lambda "x'" (Apply (Var "x'") (Var "y")))) (Var "x"), Lambda "x''" (Apply (Var "x''") (Var "x")))  -- (\y. \x'. x' y) x -> \x''. x'' x
          , (Apply (Lambda "x'" (Lambda "x" (Apply (Var "x''") (Var "x'")))) (Var "x"), Lambda "x'''" (Apply (Var "x''") (Var "x")))  -- (\x'. \x. x'' x') x -> \x'''. x'' x
          ] $ \(expression, result) -> it ("reducts " ++ show expression) $ reduct expression `shouldBe` Just result

    forM_ [ Var "x"  -- x
          , Lambda "x" (Var "x")  -- \x. x
          , Lambda "x" (Var "y")  -- \x. y
          , Apply (Var "x") (Var "x")  -- x x
          ] $ \expression -> it ("can't reduct " ++ show expression) $ reduct expression `shouldBe` Nothing

  describe "alphaEquiv" $ do
    forM_ [ (Var "x", Var "x")  -- x = x
          , (Lambda "x" (Var "x"), Lambda "x" (Var "x"))  -- \x. x = \x. x
          , (Lambda "x" (Var "x"), Lambda "y" (Var "y"))  -- \x. x = \y. y
          , (Lambda "x" (Var "y"), Lambda "z" (Var "y"))  -- \x. y = \z. y
          , (Lambda "x" (Lambda "x" (Var "x")), Lambda "y" (Lambda "z" (Var "z")))  -- \x. \x. x = \y. \z. z
          , (Lambda "a" (Lambda "b" (Var "b")), Lambda "c" (Lambda "d" (Var "d")))  -- \a. \b. b = \c. \d. d
          , (Lambda "x" (Apply (Var "x") (Var "x")), Lambda "y" (Apply (Var "y") (Var "y")))  -- \x. x x = \y. y y
          ] $ \(e1, e2) -> it ("returns True when given alpha-equivalant expressions " ++ show e1 ++ " and " ++ show e2) $ alphaEquiv e1 e2 `shouldBe` True

    forM_ [ (Var "x", Var "y")  -- x /= y
          , (Lambda "x" (Var "x"), Lambda "x" (Var "y"))  -- \x. x /= \x. y
          , (Lambda "x" (Var "x"), Lambda "y" (Var "x"))  -- \x. x /= \y. x
          , (Lambda "x" (Lambda "x" (Var "y")), Lambda "y" (Lambda "z" (Var "y")))  -- \x. \x. y /= \y. \z. y
          , (Lambda "x" (Lambda "x" (Var "y")), Lambda "z" (Lambda "y" (Var "y")))  -- \x. \x. y /= \z. \y. y
          , (Var "x", Lambda "y" (Var "x"))  -- x /= \y. x
          ] $ \(e1, e2) -> it ("returns False when given non-alpha-equivalant expressions " ++ show e1 ++ " and " ++ show e2) $ alphaEquiv e1 e2 `shouldBe` False