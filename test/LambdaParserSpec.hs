module LambdaParserSpec
  ( main
  , spec
  ) where

import Test.Hspec

import LambdaParser
import BetaReductor
import Control.Monad
import Text.Parsec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "lambdaParser" $ do
    forM_ [ ("x", Var "x")
          , ("a b", Apply (Var "a") (Var "b"))
          , ("a  b", Apply (Var "a") (Var "b"))
          , (" a b", Apply (Var "a") (Var "b"))
          , ("\\x.x", Lambda "x" (Var "x"))
          , ("\\x . x", Lambda "x" (Var "x"))
          , ("\\x -> x", Lambda "x" (Var "x"))
          , ("λ x. x", Lambda "x" (Var "x"))
          , ("(x)", Var "x")
          , ("(a b)", Apply (Var "a") (Var "b"))
          , ("( a b )", Apply (Var "a") (Var "b"))
          , ("a b c", Apply (Apply (Var "a") (Var "b")) (Var "c"))
          , ("a (b c)", Apply (Var "a") (Apply (Var "b") (Var "c")))
          , ("a (b (c d))", Apply (Var "a") (Apply (Var "b") (Apply (Var "c") (Var "d"))))
          ] $ \(s, l) -> it ("accepts " ++ s) $ parse lambdaParser "" s `shouldBe` Right l

    let isLeft = either (const True) (const False)
    forM_ [ ""
          , "."
          , "(x"
          ] $ \s -> it ("rejects " ++ s) $ isLeft (parse lambdaParser "" s) `shouldBe` True
