module Language.NLambda.ParserSpec
  ( main
  , spec
  ) where

import Test.Hspec

import Language.NLambda
import Language.NLambda.Parser

import Control.Monad (forM_)
import Text.Parsec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "lambdaParser" $ do
    forM_
      [ ("x", Var "x")
      , ("a b", Var "a" >-> Var "b")
      , ("a  b", Var "a" >-> Var "b")
      , (" a b", Var "a" >-> Var "b")
      , ("a b ", Var "a" >-> Var "b")
      , ("a\nb", Var "a" >-> Var "b")
      , ("a#b\nc", Var "a" >-> Var "c")
      , ("\\x.x", Lambda "x" (Var "x"))
      , ("\\x . x", Lambda "x" (Var "x"))
      , ("\\x -> x", Lambda "x" (Var "x"))
      , ("λ x. x", Lambda "x" (Var "x"))
      , ("λ あ. あ", Lambda "あ" (Var "あ"))
      , ("λ abc. def ghi", Lambda "abc" (Var "def" >-> Var "ghi"))
      , ("\\x y. x y", Lambda "x" (Lambda "y" (Var "x" >-> Var "y")))
      , ("(x)", Var "x")
      , ("(a b)", Var "a" >-> Var "b")
      , ("( a b )", Var "a" >-> Var "b")
      , ("a b c", Var "a" >-> Var "b" >-> Var "c")
      , ("a (b c)", Var "a" >-> (Var "b" >-> Var "c"))
      , ("a (b (c d))", Var "a" >-> (Var "b" >-> (Var "c" >-> Var "d")))
      , ("let x = a in x", Lambda "x" (Var "x") >-> Var "a")
      , ("let x = x in x", Lambda "x" (Var "x") >-> Var "x")
      , ("let x = a in let y = b in c", Lambda "x" (Lambda "y" (Var "c") >-> Var "b") >-> Var "a")
      , ("let x = a, y = b in c", Lambda "x" (Lambda "y" (Var "c") >-> Var "b") >-> Var "a")
      , ("a let x = b in c d", Var "a" >-> (Lambda "x" (Var "c" >-> Var "d") >-> Var "b"))
      , ("let x = let y = a in b in c", Lambda "x" (Var "c") >-> (Lambda "y" (Var "b") >-> Var "a"))
      ] $ \(s, e) ->
        it ("accepts " ++ s) $ parse (lambdaParser <* eof) "" s `shouldBe` Right e

    let toPos = either (sourceColumn . errorPos) (const 0)
    forM_
      [ ("", 1)
      , (".", 1)
      , ("(x", 3)
      , ("(x))", 4)
      , ("λ x. λ", 7)
      ] $ \(s, pos) ->
        it ("rejects " ++ s) $ toPos (parse (lambdaParser <* eof) "" s) `shouldBe` pos
