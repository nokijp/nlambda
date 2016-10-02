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
      [ ( 10
        , Var "a"
        , [Result $ Var "a"]
        )
      , ( 0
        , Var "a"
        , [Error Complicated]
        )
      , ( 10
        , Lambda "x" (Var "x") <> Var "a"
        , [Step $ Lambda "x" (Var "x") <> Var "a", Result $ Var "a"]
        )
      , ( 10
        , Lambda "x" (Var "x" <> Var "x") <> Lambda "x" (Var "x" <> Var "x")
        , [Loop $ Lambda "x" (Var "x" <> Var "x") <> Lambda "x" (Var "x" <> Var "x")]
        )
      , ( 10
        , Lambda "x" (Var "x" <> Var "x" <> Var "x") <> Lambda "x" (Var "x" <> Var "x" <> Var "x")
        , [Step $ Lambda "x" (Var "x" <> Var "x" <> Var "x") <> Lambda "x" (Var "x" <> Var "x" <> Var "x"), Error Complicated]
        )
      ] $ \(maxSize, e, result) ->
        it ("returns " ++ show result ++ " when given " ++ show e) $ steps maxSize e `shouldBe` result

  describe "runLambda" $ do
    forM_
      [ (10, 10, Var "a", Result $ Var "a")
      , (10, 0, Var "a", Error Complicated)
      , (10, 10, Lambda "x" (Var "x") <> Var "a", Result $ Var "a")
      , (10, 1, Lambda "x" (Var "x") <> Var "a", Error Complicated)
      , (10, 10, Lambda "x" (Var "x" <> Var "x") <> Lambda "x" (Var "x" <> Var "x"), Loop $ Lambda "x" (Var "x" <> Var "x") <> Lambda "x" (Var "x" <> Var "x"))
      , (10, 10, Lambda "x" (Var "x" <> Var "x" <> Var "x") <> Lambda "x" (Var "x" <> Var "x" <> Var "x"), Error Complicated)
      , (100, 2, Lambda "x" (Var "x" <> Var "x" <> Var "x") <> Lambda "x" (Var "x" <> Var "x" <> Var "x"), Error Complicated)
      ] $ \(maxSteps, maxSize, e, result) ->
        it ("returns " ++ show result ++ " when given " ++ show e) $ runLambda maxSteps maxSize e `shouldBe` result
