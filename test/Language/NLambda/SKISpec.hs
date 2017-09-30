module Language.NLambda.SKISpec
  ( main
  , spec
  ) where

import Test.Hspec

import qualified Language.NLambda as L
import Language.NLambda.Reductor
import Language.NLambda.SKI

import Control.Monad (forM_)

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

  describe "skiString" $ do
    forM_
      [ (Var "x", "x")
      , (K <> I, "K I")
      , (S <> K <> I, "S K I")
      , (S <> (K <> K), "S (K K)")
      , (S <> (K <> K) <> I, "S (K K) I")
      ] $ \(e, s) ->
        it ("converts " ++ show e ++ " to " ++ s) $ skiString e `shouldBe` s

  describe "skiToLambda" $ do
    forM_
      [ (Var "x", L.Var "x")
      , (S <> (K <> K) <> I, (L.Lambda "x" $ L.Lambda "y" $ L.Lambda "z" $ L.Var "x" L.<> L.Var "z" L.<> (L.Var "y" L.<> L.Var "z")) L.<> ((L.Lambda "x" $ L.Lambda "y" $ L.Var "x") L.<> (L.Lambda "x" $ L.Lambda "y" $ L.Var "x")) L.<> L.Lambda "x" (L.Var "x"))
      , (S <> I <> I <> (S <> I <> I), (L.Lambda "x" $ L.Lambda "y" $ L.Lambda "z" $ L.Var "x" L.<> L.Var "z" L.<> (L.Var "y" L.<> L.Var "z")) L.<> L.Lambda "x" (L.Var "x") L.<> L.Lambda "x" (L.Var "x") L.<> ((L.Lambda "x" $ L.Lambda "y" $ L.Lambda "z" $ L.Var "x" L.<> L.Var "z" L.<> (L.Var "y" L.<> L.Var "z")) L.<> L.Lambda "x" (L.Var "x") L.<> L.Lambda "x" (L.Var "x")))
      ] $ \(ski, lambda) ->
        it ("converts " ++ show ski ++ " to " ++ show lambda) $ skiToLambda ski `shouldSatisfy` alphaEquiv lambda

  describe "lambdaToSki" $ do
    forM_
      [ (L.Var "x", Var "x")
      , (L.Var "x" L.<> L.Var "y", Var "x" <> Var "y")
      , (L.Lambda "x" $ L.Var "x", I)
      , (L.Lambda "x" $ L.Var "y", K <> Var "y")
      , (L.Lambda "x" $ L.Var "x" L.<> L.Var "x", S <> I <> I)
      , (L.Lambda "x" $ L.Var "x" L.<> L.Var "y", S <> I <> (K <> Var "y"))
      , (L.Lambda "x" $ L.Var "y" L.<> L.Var "x", Var "y")
      , (L.Lambda "x" $ L.Var "y" L.<> L.Var "y", K <> (Var "y" <> Var "y"))
      , (L.Lambda "x" $ L.Var "x" L.<> L.Lambda "z" (L.Var "x"), S <> I <> K)
      , (L.Lambda "x" $ L.Var "x" L.<> L.Lambda "z" (L.Var "y"), S <> I <> (K <> (K <> Var "y")))
      , (L.Lambda "x" $ L.Var "y" L.<> L.Lambda "z" (L.Var "x"), S <> (K <> Var "y") <> K)
      , (L.Lambda "x" $ L.Var "y" L.<> L.Lambda "z" (L.Var "y"), K <> (Var "y" <> (K <> Var "y")))
      , (L.Lambda "x" $ L.Lambda "y" $ L.Var "x", K)
      , (L.Lambda "x" $ L.Lambda "y" $ L.Var "y", K <> I)
      , (L.Lambda "x" $ L.Lambda "y" $ L.Var "z", K <> (K <> Var "z"))
      , (L.Lambda "x" $ L.Lambda "y" $ L.Lambda "z" $ L.Var "x" L.<> L.Var "z" L.<> (L.Var "y" L.<> L.Var "z"), S)
      , ((L.Lambda "x" $ L.Var "x" L.<> L.Var "x") L.<> (L.Lambda "x" $ L.Var "x" L.<> L.Var "x"), S <> I <> I <> (S <> I <> I))
      ] $ \(lambda, ski) ->
        it ("converts " ++ show lambda ++ " to " ++ show ski) $ lambdaToSki lambda `shouldBe` ski
