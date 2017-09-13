module Pins (
    mkPins
  , mkRange
  , hasConsecElems
  , hasIncConsecElems
  , Digit(..)
  ) where

import Prelude
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Function (on)
import Data.Array as A
import Data.Enum
import Data.Foldable (all, foldl)
import Data.Traversable (traverse, sequence)
import Data.Tuple.Nested ((/\))
import Data.String as Str
import Data.Newtype (wrap)
import Data.List (List(..), (:), fromFoldable, take, length, mapWithIndex, foldM, range)
import Data.Int as Int
import Data.Set (Set)
import Data.Set as Set
import Data.Monoid (mempty)
import Control.MonadZero (guard)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (throwException, error)
import Control.Monad.Eff.Random (randomInt)
import Control.Monad.Rec.Class (tailRecM, Step(Loop, Done))

data Digit = Zero | One | Two | Three | Four | Five | Six | Seven | Eight | Nine

digitToInt :: Digit -> Int
digitToInt Zero  = 0
digitToInt One   = 1
digitToInt Two   = 2
digitToInt Three = 3
digitToInt Four  = 4
digitToInt Five  = 5
digitToInt Six   = 6
digitToInt Seven = 7
digitToInt Eight = 8
digitToInt Nine  = 9

intToDigit :: Int -> Maybe Digit
intToDigit 0 = pure Zero
intToDigit 1 = pure One
intToDigit 2 = pure Two
intToDigit 3 = pure Three
intToDigit 4 = pure Four
intToDigit 5 = pure Five
intToDigit 6 = pure Six
intToDigit 7 = pure Seven
intToDigit 8 = pure Eight
intToDigit 9 = pure Nine
intToDigit _ = Nothing

instance ordDigit :: Ord Digit where compare = compare `on` digitToInt
instance eqDigit :: Eq Digit where eq = eq `on` digitToInt
instance enumDigit :: Enum Digit where
  pred = intToDigit <<< (_ - 1) <<< digitToInt
  succ = intToDigit <<< (_ + 1) <<< digitToInt

type Pin = List Digit

mkPin :: Int -> Eff _ Pin
mkPin r = do
  i <- randomInt 0 $ mkRange r
  case traverse (intToDigit <=< Int.fromString)
          $ Str.split (wrap "") (padLeft '0' r $ show i) of
    Just digits -> pure $ fromFoldable digits
    _ -> throwException $ error $ "Generated invalid digit"

mkPins :: Int -> Int -> Eff _ (List Pin)
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
            (Set.insert pin acc) /\ (n - 1) <$ do
              guard $ not $ hasConsecElems 2 pin
              guard $ not $ hasIncConsecElems 3 pin

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

hasIncConsecElems :: ∀ a. Eq a => Enum a => Int -> List a -> Boolean
hasIncConsecElems n _ | n <= 1 = false
hasIncConsecElems _ Nil = false
hasIncConsecElems n xs = go xs
  where
  go Nil = false
  go (x:xs)
    | let ys = take (n - 1) xs
          ys' = sequence $ flip mapWithIndex ys \i e ->
                  foldl (\a _ -> a >>= pred) (pure e) (range 0 i)
       in length ys >= n - 1 &&
            case ys' of
              Just ys'' -> all (_ == x) ys''
              _ -> false
    = true
  go (_:xs) = go xs

mkRange :: Int -> Int
mkRange size = Int.pow 10 size - 1

padLeft :: Char -> Int -> String -> String
padLeft c len str = prefix <> str
  where prefix = Str.fromCharArray $ A.replicate (len - Str.length str) c
