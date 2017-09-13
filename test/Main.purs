module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (throwException, error)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Data.Traversable (for)
import Data.Newtype (wrap)
import Data.String as S
import Data.List.Lazy (replicateM)
import Data.List (fromFoldable, length)
import Data.Foldable (traverse_, for_)
import Test.Spec (pending, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)
import Pins

main :: Eff _ Unit
main = run [consoleReporter] do

  describe "hasConsecElems" do
    for_
        [ ([1,1,2] /\ 2 /\ true)
        , ([1,2] /\ 2 /\ false)
        , ([1,2,1] /\ 2 /\ false)
        , ([] /\ 2 /\ false)
        , ([1,1,1] /\ 2 /\ true)
        , ([1,1,1] /\ 4 /\ false)
        , ([1,1,1,2] /\ 2 /\ true)
        , ([1,0,1,2,2] /\ 2 /\ true)
        ] \(input /\ consec /\ expected) -> do
      it (show input <> " -> " <> show consec <> " -> " <> show expected) do
        hasConsecElems consec (fromFoldable input) `shouldEqual` expected

  describe "hasIncConsecElems" do
    for_
        [ ([1,1,2] /\ 2 /\ true)
        , ([1,2] /\ 2 /\ true)
        , ([] /\ 2 /\ false)
        , ([1,1,1] /\ 2 /\ false)
        , ([1,1,1,2] /\ 2 /\ true)
        , ([1,0,1,2,2] /\ 2 /\ true)
        , ([2,2,0,1,2] /\ 2 /\ true)
        , ([2,1,0,2,2] /\ 2 /\ false)
        , ([2,1,0,2,2] /\ 1 /\ false)
        , ([2,1,0,2,2] /\ 0 /\ false)
        ] \(input /\ consec /\ expected) -> do
      it (show input <> " -> " <> show consec <> " -> " <> show expected) do
        hasIncConsecElems consec (fromFoldable input) `shouldEqual` expected

  describe "mkRange" do
    it "should produce correct output for size: 1" do
      mkRange 1 `shouldEqual` 9

    it "should produce correct output for size: 2" do
      mkRange 2 `shouldEqual` 99

    it "should produce correct output for size: 3" do
      mkRange 3 `shouldEqual` 999

    it "should produce correct output for size: 4" do
      mkRange 4 `shouldEqual` 9999

  describe "mkPins" do
    it "makes pins of the right shape" do
      replicateM 10 (liftEff $ mkPins 4 10) >>= traverse_ \pins -> do
        length pins `shouldEqual` 10
        for_ pins \pin -> do
          let xs = fromFoldable $ S.split (wrap "") pin
          length xs `shouldEqual` 4

          ys <- for xs \x ->
            case Int.fromString x of
              Just v -> pure v
              Nothing -> liftEff do
                throwException $ error $ "invalid pin member: " <> show x

          when (hasIncConsecElems 3 ys) $ liftEff do
             throwException $ error $ "detected consecutive incrementing integer"

          when (hasConsecElems 2 ys) $ liftEff do
             throwException $ error $ "detected consecutive equal integers"
