module Language.NLambda.InterpreterSpec
  ( main
  , spec
  ) where

import Test.Hspec

import Language.NLambda
import Language.NLambda.Interpreter

import Control.Monad (forM_)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "steps" $ do
    forM_
      [ ( Var "a"
        , 10
        , [Result $ Var "a"]
        )
      , ( Var "a"
        , 0
        , [Error Complicated]
        )
      , ( Lambda "x" (Var "x") <> Var "a"
        , 10
        , [Step $ Lambda "x" (Var "x") <> Var "a", Result $ Var "a"]
        )
      , ( Lambda "x" (Var "x" <> Var "x") <> Lambda "x" (Var "x" <> Var "x")
        , 10
        , [Loop $ Lambda "x" (Var "x" <> Var "x") <> Lambda "x" (Var "x" <> Var "x")]
        )
      , ( Lambda "x" (Var "x" <> Var "x" <> Var "x") <> Lambda "x" (Var "x" <> Var "x" <> Var "x")
        , 10
        , [Step $ Lambda "x" (Var "x" <> Var "x" <> Var "x") <> Lambda "x" (Var "x" <> Var "x" <> Var "x"), Error Complicated]
        )
      ] $ \(e, maxSize, result) ->
        it ("returns reduction steps when given " ++ show e) $ steps maxSize e `shouldBe` result
