module Pins (
    mkPins
  , mkRange
  , hasConsecElems
  , hasIncConsecElems
  ) where

import Prelude
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Array as A
import Data.Foldable (all)
import Data.Traversable (traverse)
import Data.Tuple.Nested ((/\))
import Data.String as Str
import Data.Newtype (wrap)
import Data.List (List(..), (:), fromFoldable, take, length, mapWithIndex)
import Data.Int as Int
import Data.Set (Set)
import Data.Set as Set
import Data.Monoid (mempty)
import Control.MonadZero (guard)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (randomInt)
import Control.Monad.Rec.Class (tailRecM, Step(Loop, Done))

mkPin :: Int -> Eff _ String
mkPin r = do
  i <- randomInt 0 $ mkRange r
  pure $ padLeft '0' r $ show i

mkPins :: Int -> Int -> Eff _ (List String)
mkPins size n = fromFoldable <$> tailRecM go (mempty /\ n)
  where
  go (acc /\ n) | n <= 0 = pure $ Done acc
  go (acc /\ n) = do
    pin <- mkPin size
    pure
      $ Loop
      $ if pin `Set.member` acc
          then acc /\ n
          else fromMaybe (acc /\ n) $
            let xs = fromFoldable $ Str.split (wrap "") pin
            in (Set.insert pin acc) /\ (n - 1) <$ do
                  ys <- traverse Int.fromString xs
                  guard $ not $ hasConsecElems 2 ys
                  guard $ not $ hasIncConsecElems 3 ys

hasConsecElems :: ∀ a. Eq a => Int -> List a -> Boolean
hasConsecElems n _ | n <= 1 = false
hasConsecElems n x = go x
  where
  go Nil = false
  go (x:xs)
    | let ys = take (n - 1) xs
       in length ys >= n - 1 && all (_ == x) ys
    = true
  go (_:xs) = go xs

hasIncConsecElems :: Int -> List Int -> Boolean
hasIncConsecElems n _ | n <= 1 = false
hasIncConsecElems n x = go x
  where
  go Nil = false
  go (x:xs)
    | let ys = take (n - 1) xs
          ys' = mapWithIndex (flip (-) <<< (_ + 1)) ys
       in length ys >= n - 1 && all (_ == x) ys'
    = true
  go (_:xs) = go xs

mkRange :: Int -> Int
mkRange size = Int.pow 10 size - 1

padLeft :: Char -> Int -> String -> String
padLeft c len str = prefix <> str
  where prefix = Str.fromCharArray $ A.replicate (len - Str.length str) c
