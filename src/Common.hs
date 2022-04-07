module Common where

import Numeric.Natural
import Data.Foldable
import Data.List (inits, tails)
import GHC.Generics (Generic)

type Nat     = Natural

--------------------------------------------------------------------------------
-- COMMON FUNCTIONS ON ORDERINGS                                              --
--------------------------------------------------------------------------------

-- | Compares two elements on a mapped value.
compareOn :: (Ord b) => (a -> b) -> a -> a -> Ordering
compareOn f x y = f x `compare` f y

--------------------------------------------------------------------------------
-- COMMON FUNCTIONS ON LISTS                                                  --
--------------------------------------------------------------------------------

-- | Finds the minimum on a mapped value.
minimumOn :: (Foldable t, Ord b) => (a -> b) -> t a -> a
minimumOn f = minimumBy (compareOn f)

-- | Converts a function on a foldable to it's nullSafe counterpart.
nullSafe :: Foldable t => (t a -> a) ->  t a -> Maybe a
nullSafe f x = if null x then Nothing else Just $ f x

-- Some safe versions of common methods.
safeHead, safeLast :: [a] -> Maybe a
safeMinimum        :: (Foldable t, Ord a) => t a -> Maybe a
safeMinimumOn      :: (Foldable t, Ord b) => (a -> b) -> t a -> Maybe a
safeMinimumBy      :: (Foldable t) => (a -> a -> Ordering) -> t a -> Maybe a
safeHead = nullSafe head
safeLast = nullSafe last
safeMinimum = nullSafe minimum
safeMinimumOn f = nullSafe (minimumOn f)
safeMinimumBy f = nullSafe (minimumBy f)
