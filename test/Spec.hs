import Test.Hspec

import BetaReductor
import Control.Monad

main :: IO ()
main = hspec $ do
  describe "reduct" $ do
    forM_ [ (Apply (Lambda "x" (Var "x")) (Var "x"), Var "x")  -- (\x. x) x -> x
          , (Apply (Lambda "x" (Var "y")) (Var "x"), Var "y")  -- (\x. y) x -> y
          , (Apply (Lambda "x" (Lambda "x" (Var "x"))) (Var "x"), Lambda "x'" (Var "x'"))  -- (\x. (\x. x)) x -> \x'. x'
          , (Apply (Lambda "y" (Lambda "x" (Var "x"))) (Var "x"), Lambda "x'" (Var "x'"))  -- (\y. (\x. x)) x -> \x'. x'
          , (Apply (Lambda "x" (Lambda "y" (Var "x"))) (Var "x"), Lambda "y" (Var "x"))  -- (\x. (\y. x)) x -> \y. x
          , (Apply (Var "x") (Apply (Lambda "y" (Var "y")) (Var "z")), Apply (Var "x") (Var "z"))  -- x ((\y. y) z) -> x z
          , (Lambda "x" (Apply (Lambda "y" (Var "y")) (Var "z")), Lambda "x" (Var "z"))  -- (\x. (\y. y) z) -> \x. z
          , (Apply (Lambda "y" (Lambda "x" (Apply (Var "x") (Var "y")))) (Var "x"), Lambda "x'" (Apply (Var "x'") (Var "x")))  -- (\y. (\x. x y)) x -> \x'. x' x
          , (Apply (Lambda "y" (Lambda "x" (Apply (Var "x'") (Var "y")))) (Var "x"), Lambda "x''" (Apply (Var "x'") (Var "x")))  -- (\y. (\x. x' y)) x -> \x''. x' x
          , (Apply (Lambda "y" (Lambda "x'" (Apply (Var "x'") (Var "y")))) (Var "x"), Lambda "x''" (Apply (Var "x''") (Var "x")))  -- (\y. (\x'. x' y)) x -> \x''. x'' x
          , (Apply (Lambda "x'" (Lambda "x" (Apply (Var "x''") (Var "x'")))) (Var "x"), Lambda "x'''" (Apply (Var "x''") (Var "x")))  -- (\x'. (\x. x'' x')) x -> \x'''. x'' x
          ] $ \(expression, result) -> it ("reducts " ++ show expression) $ reduct expression `shouldBe` Just result

    forM_ [ Var "x"
          , Lambda "x" (Var "x")
          , Lambda "x" (Var "y")
          , Apply (Var "x") (Var "x")
          ] $ \expression -> it ("can't reduct " ++ show expression) $ reduct expression `shouldBe` Nothing
