module LambdaParserSpec
  ( main
  , spec
  ) where

import Test.Hspec

import Lambda
import LambdaParser

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
          , ("a b ", Apply (Var "a") (Var "b"))
          , ("\\x.x", Lambda "x" (Var "x"))
          , ("\\x . x", Lambda "x" (Var "x"))
          , ("\\x -> x", Lambda "x" (Var "x"))
          , ("λ x. x", Lambda "x" (Var "x"))
          , ("λ あ. あ", Lambda "あ" (Var "あ"))
          , ("λ abc. def ghi", Lambda "abc" (Apply (Var "def") (Var "ghi")))
          , ("\\x y. x y", Lambda "x" (Lambda "y" (Apply (Var "x") (Var "y"))))
          , ("(x)", Var "x")
          , ("(a b)", Apply (Var "a") (Var "b"))
          , ("( a b )", Apply (Var "a") (Var "b"))
          , ("a b c", Apply (Apply (Var "a") (Var "b")) (Var "c"))
          , ("a (b c)", Apply (Var "a") (Apply (Var "b") (Var "c")))
          , ("a (b (c d))", Apply (Var "a") (Apply (Var "b") (Apply (Var "c") (Var "d"))))
          ] $ \(s, e) -> it ("accepts " ++ s) $ parse (lambdaParser <* eof) "" s `shouldBe` Right e

    let toPos = either (sourceColumn . errorPos) (const 0)
    forM_ [ ("", 1)
          , (".", 1)
          , ("(x", 3)
          , ("(x))", 4)
          , ("λ x. λ", 7)
          ] $ \(s, pos) -> it ("rejects " ++ s) $ toPos (parse (lambdaParser <* eof) "" s) `shouldBe` pos
