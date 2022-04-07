{-# LANGUAGE FlexibleInstances
           , OverloadedStrings
           , DeriveFunctor
           , UndecidableInstances
           , TypeFamilies
           , FlexibleContexts
           , DefaultSignatures
           , DuplicateRecordFields
           , DeriveGeneric
           , DataKinds
           , TypeApplications #-}

module Types where

import Common
import Prettyprinter
import Numeric.Natural
import GHC.Arr hiding (join)
import Data.Default
import Data.List hiding (splitAt)
import qualified Data.List as L
import qualified Data.Foldable as F (toList)
import Control.Comonad.Store (ComonadStore)
import qualified Control.Comonad.Store as C
import Control.Lens hiding ((...))
import GHC.Exts (IsList)
import GHC.Generics hiding ((:*:), to)
import Data.Generics.Labels
import Data.Generics.Product as GP
import Data.Bool
import GHC.OverloadedLabels hiding (join)

--------------------------------------------------------------------------------
-- FUNCTION DIMENSIONS                                                        --
--------------------------------------------------------------------------------

-- | Typeclass for types to represent the dimensions of a function.
class FuncDims a where
  n :: a -> Nat -- ^ The input dimensions.
  n = n . toDims
  m :: a -> Nat -- ^ The output dimensions.
  m = m . toDims
  toDims :: a -> Dims -- ^ Convert to the standard Dims type.
  toDims x = n x :-> m x
  {-# MINIMAL (m,n) | toDims #-}

-- STANDARD IMPLEMENTATION

-- | Standard datatype to represent Dims.
data Dims = Nat :-> Nat deriving (Show, Read, Eq)


-- Implementation of standard representation.
instance FuncDims Dims where
  toDims = id
  n (n' :-> _ ) = n'
  m (_  :-> m') = m'

-- SMART CONSTRUCTORS

-- | Converts a list of numbers to a list of dimensions.
zipDims :: [Nat] -> [Dims]
zipDims [] = []
zipDims [x] = [x :-> x]
zipDims xs = zipWith (:->) xs (tail xs)

-- LENSES

-- | Lenses for the dimension of the input (`_n`) and output (`_m`).
_n, _m :: Lens' Dims Nat
_n f x = (:-> m x) <$> f (n x)
_m f x = (n x :->) <$> f (m x)

--------------------------------------------------------------------------------
-- FUNCTION DESCRIPTIONS                                                      --
--------------------------------------------------------------------------------

-- | Typeclass for types that represent the edges of a function.
class FuncDims a => FuncDesc a where
  edges :: a -> Nat -- ^ The amount of edges for the DAG of a function.
  edges = edges . toF
  toF :: a -> F -- ^ Converts to the standard F type.
  toF x = F (toDims x) (edges x)
  {-# MINIMAL toF | edges #-}

-- STANDARD IMPLEMENTATION

-- | Standard datatype to represent a FuncDesc.
data F = F Dims Nat deriving (Show, Read, Eq)

instance FuncDims F where
  toDims (F dims _) = dims

instance FuncDesc F where
  toF = id
  edges (F _ edges') = edges'

-- SMART CONSTRUCTORS

-- | Creates a list of `F` objects from two lists of numbers.
zipF :: [Nat] -> [Nat] -> [F]
zipF = zipWith F . zipDims

-- LENSES

-- | Lens to the amount of edges.
_edges :: Lens' F Nat
_edges f x = F (toDims x) <$> f (edges x)

-- | Lens to the dimensions
_dims :: Lens' F Dims
_dims f x = (`F` edges x) <$> f (toDims x)

--------------------------------------------------------------------------------
-- FUNCTION COMPOSITION INDICES                                               --
--------------------------------------------------------------------------------

infix 4 /\
infix 4 \/

class Bounded a => Lattice a where
  meet, join :: Foldable t => t a -> a
  meet = foldr (/\) maxBound
  join = foldr (\/) minBound
  (/\), (\/) :: a -> a -> a
  lhs /\ rhs = meet [lhs,rhs]
  lhs \/ rhs = join [lhs,rhs]
  {-# MINIMAL (meet | (/\)), (join | (\/)) #-}

infix 5 :...:
infix 5 ...
infix 5 ..<
infix 5 <..
infix 5 ..+
infix 5 +..

data FIx = Int :...: Int deriving (Show, Read, Generic)

(...), (..<), (<..), (..+), (+..) :: Int -> Int -> FIx
i ... j | i <= j    = i :...: j
        | otherwise = minBound
i ..< j = i ... j - 1
i <.. j = i + 1 ... j
i ..+ l = i ..< i + l
l +.. j = j - l <.. j

-- | Sets the size of an FIx by changing the `lower` (resizeL) or the `upper`
--   (resizeR) value.
resizeL, resizeR :: Int -> FIx -> FIx
resizeL l (_ :...: j) = j - l <.. j
resizeR l (i :...: _) = i ..< i + l

-- | Shifts both the `upper` and `lower` value of an FIx by the given amount.
shiftBy :: Int -> FIx -> FIx
shiftBy delta (i :...: j) = i + delta ... j + delta

pow, pow0 :: FIx -> [FIx]
pow  x@(i :...: j) = [i'...j' | i' <- [i..j], j' <- [i'..j]]
pow0 x = minBound:pow x

powSize :: Int -> FIx -> [FIx]
powSize l x@(i :...: _) = [i'..+l | i' <- [i..i+size x - l]]

-- RANGED TYPECLASS

-- | Typeclass for objects that have an FIx.
class Ranged a where
  -- | Get the `lower`-bound, `upper`-bound and the `size` of the object.
  lower, upper, size :: a -> Int
  lower = lower . whole
  upper = upper . whole
  size  = size  . whole
  -- | Gives the whole range of the object as an FIx.
  whole :: a -> FIx
  whole x = lower x ..< size x
  -- | Gives if the range of the object is empty.
  isEmpty :: a -> Bool
  isEmpty x = upper x < lower x
  {-# MINIMAL (whole | (size, lower)) #-}

instance Ranged FIx where
  upper (_ :...: j') = j'
  lower (i :...: _) = i
  size (i :...: j) = max 0 (j - i + 1)
  whole = id

-- LENSES

_sizeL, _sizeR, _lower, _upper :: Lens' FIx Int
_sizeL f i = (`resizeL` i) <$> f (size i)
_sizeR f i = (`resizeR` i) <$> f (size i)
_lower f i = (... upper i) <$> f (lower i)
_upper f i = (lower i ...) <$> f (upper i)

empty :: Lens' FIx Bool
empty f i = bool i minBound <$> f (isEmpty i)

-- | Checks if the first Ranged object has a subrange of the second Ranged
--   object.
isSubRangeOf :: (Ranged a, Ranged b) => a -> b -> Bool
x `isSubRangeOf` y = lower x >= lower y && upper x <= upper y

-- SLICEABLE TYPECLASS

-- | Typeclass for types that can be slices into smaller pieces using an FIx.
class Sliceable a where
  -- | Slices the object in smaller parts.
  slice :: FIx -> a -> a

instance Sliceable [a] where
  slice (i :...: j) = drop (i-1) . take j

instance Sliceable FIx where
  slice = (/\)

-- | Splits the object into two smaller parts. The first argument determines
--   the size of the first part.
splitAt' :: Int -> FIx -> (FIx, FIx)
splitAt' n (i :...: j) = (i ..< i+n, i+n ... j)

splits' :: FIx -> [(FIx, FIx)]
splits' x = [splitAt' k x | k <- [1..size x - 1]]

-- OTHER INSTANCES

instance Eq FIx where
  x == y = (isEmpty x && isEmpty y) || (lower x == lower y && upper x == upper y)

instance Ord FIx where
  compare x y | isEmpty x && isEmpty y = EQ
              | otherwise = compare (size x) (size y)
                            <> compare (lower x) (lower y)

instance Ix FIx where
  range (l,u) = takeWhile (<= u) $ dropWhile (<l) $ sort (pow (l \/ u))
  unsafeIndex (l,u) x = (lower x - lower l) + (size x - size l) * size (l \/ u)
  inRange (l,u) x = x >= l && x <= u && x `isSubRangeOf` (l \/ u)

instance Bounded FIx where
  minBound = maxBound :...: minBound
  maxBound = minBound :...: maxBound

instance Lattice FIx where
  (xi :...: xj) /\ (yi :...: yj) = max xi yi ... min xj yj
  (xi :...: xj) \/ (yi :...: yj) = min xi yi ... max xj yj

instance Default FIx where
  def = 1...1

-- Implementation of slices.

instance Ranged (FIx, a) where
  whole = fst

instance FuncComp a => FuncDims (FIx, a) where
  toDims = toDims . toFnComp

instance FuncComp a => FuncDesc (FIx, a) where
  toF = toF . toFnComp

instance FuncComp a => FuncComp (FIx, a) where
  toFnComp (i, fs) = slice i $ toFnComp fs

--------------------------------------------------------------------------------
-- FUNCTION COMPOSITIONS                                                      --
--------------------------------------------------------------------------------

-- Class for types that represent function compositions.
class FuncDesc a => FuncComp a where
  -- | Converts to a standard list of functions.
  toFnComp :: a -> [F]
  -- | Gets some sub-function composition.
  subFnComp :: FIx -> a -> [F]
  subFnComp i fs = slice i $ toFnComp fs

  default toFnComp :: Solution a => a -> [F]
  toFnComp = toFnComp . toBrak

-- Class for types that can be constructed using a function composition.
class FuncComp a => IsFuncComp a where
  -- | Constructs from a standard list of functions.
  fromFnComp :: [F] -> a

-- | Dimensions of a function composition.
instance FuncDims a => FuncDims [a] where
  n = maybe 0 n . safeHead
  m = maybe 0 m . safeLast

-- | Edges of a function composition.
instance FuncDesc a => FuncDesc [a] where
  edges fs = sum (edges <$> fs)

instance FuncDesc a => FuncComp [a] where
  toFnComp = fmap toF

instance IsFuncComp [F] where
  fromFnComp = id

--------------------------------------------------------------------------------
-- SOLUTIONS AND BRACKETINGS                                                  --
--------------------------------------------------------------------------------

-- | Class representing a solution to a bracketing problem.
class FuncComp a => Solution a where
  toBrak :: a -> Brak -- ^ The bracketing that represents the solution.
  mem  :: a -> Nat -- ^ The memory required a solution.
  mem = mem . toBrak
  fma  :: a -> Nat -- ^ The fma-cost a solution.
  fma = fma . toBrak

-- | Compare solution by it's `fma` first and it's `mem` second.
scompare :: (Solution a, Solution b) => a -> b -> Ordering
scompare x y = compare (fma x) (fma y) <> compare (mem x) (mem y)

-- | Finds the minimum element using the ordering defined by `scompare`.
sminimum :: (Foldable t, Solution a) => t a -> Maybe a
sminimum = safeMinimumBy scompare

-- BRACKETINGS

-- | An algebraic datatype that represents bracketings.
data Bracketing a = I Nat                          -- ^ Initiation (Identity)
                  | a            :>: Bracketing a  -- ^ Tangent mode
                  | Bracketing a :<: a             -- ^ Adjoint mode
                  | Bracketing a :*: Bracketing a  -- ^ Preaccumulation
                  deriving (Show, Read, Functor)

infixr 8 :>: -- Fixity for tangent mode.
infixl 8 :<: -- Fixity for adjoint mode.
infixl 7 :*: -- Fixity for preaccumulation.

-- | The default representation of a solution to a bracketing problem.
type Brak = Bracketing F

-- SMART CONSTRUCTORS
-- | Makes a homogenious tangent and adjoint bracketings.
tngt, adj  :: FuncDims a => [a] -> Bracketing a
tngt fs = tngtAfter fs (I (n fs))
adj  fs = adjAfter  fs (I (m fs))

-- | Appends functions in homogenious tangent and adjoint mode.
tngtAfter, adjAfter :: [a] -> Bracketing a -> Bracketing a
tngtAfter [] b = b
tngtAfter fs b = last fs :>: tngtAfter (init fs) b
adjAfter [] b = b
adjAfter fs b = adjAfter (tail fs) b :<: head fs

-- Implementations for convenient instances.
instance Foldable Bracketing where
  null (I _) = True
  null _     = False
  foldMap _ (I _) = mempty
  foldMap f (lhs :>: rhs) = foldMap f rhs <> f lhs
  foldMap f (lhs :<: rhs) = f rhs <> foldMap f lhs
  foldMap f (lhs :*: rhs)  = foldMap f rhs <> foldMap f lhs

instance Traversable Bracketing where
  traverse _ (I n) = pure (I n)
  traverse f (lhs :>: rhs) = flip (:>:) <$> traverse f rhs <*> f lhs
  traverse f (lhs :<: rhs) = flip (:<:) <$> f rhs <*> traverse f lhs
  traverse f (lhs :*: rhs) = flip (:*:) <$> traverse f rhs <*> traverse f lhs

instance FuncDesc a => Eq (Bracketing a) where
  x == y = fma x == fma y && mem x == mem y

instance FuncDesc a => Ord (Bracketing a) where
  compare = scompare

instance FuncDims a => FuncDims (Bracketing a) where
  n (I n') = n'
  n (_ :>: rhs) = n rhs
  n (_ :<: rhs) = n rhs
  n (_ :*: rhs) = n rhs
  m (I m') = m'
  m (lhs :>: _) = m lhs
  m (lhs :<: _) = m lhs
  m (lhs :*: _) = m lhs

instance FuncDesc a => FuncDesc (Bracketing a) where
  edges (I _) = 0
  edges (lhs :>: rhs) = edges lhs + edges rhs
  edges (lhs :<: rhs) = edges lhs + edges rhs
  edges (lhs :*: rhs) = edges lhs + edges rhs

instance FuncDesc a => FuncComp (Bracketing a) where
  toFnComp = fmap toF . F.toList

instance FuncDesc a => Solution (Bracketing a) where
  toBrak = fmap toF
  fma (I _) = 0
  fma (lhs :>: rhs) = fma rhs + n rhs * edges lhs
  fma (lhs :<: rhs) = fma lhs + m lhs * edges rhs
  fma (lhs :*: rhs) = fma rhs + fma lhs +  n rhs * n lhs * m lhs
  mem (I n) = n
  mem (lhs :>: rhs) = mem rhs
  mem (lhs :<: rhs) = mem lhs + edges rhs
  mem (lhs :*: rhs) = max (mem lhs + m lhs * n rhs) (mem rhs)


-- MODES AND STRATEGIES

data Mode = ADJ|TNGT|PREACC deriving (Show, Read, Eq, Enum)

--------------------------------------------------------------------------------
-- MEMORY CONSTRAINTS                                                         --
--------------------------------------------------------------------------------

-- | Types that encode a memory constraint.
class MemConstraint a where
  memConstraint :: a -> MemConstr

  default memConstraint :: (HasField' "memConstr" a MemConstr) => a -> MemConstr
  memConstraint = view (field' @"memConstr")

instance MemConstraint Integer where
  memConstraint x = Fin $ fromInteger (max 0 x)

instance MemConstraint Nat where
  memConstraint x = Fin x

instance MemConstraint MemConstr where
  memConstraint = id

-- DEFAULT IMPLEMENTATION

-- | Algebraic datatype that represents a memory constraint.
data MemConstr = Fin Nat -- ^ Finite, when there is a memory constraint.
               | Inf     -- ^ Infinite, when there is no memory constraint.
               deriving (Show, Read, Eq)

-- | Gives the highest memory constraint that does not allow the provided
--   function.
nextMem :: Solution a => a -> MemConstr
nextMem = Fin . mem

-- | Define an ordering simular to that of the extended natural numbers.
instance Ord MemConstr where
  compare Inf     Inf     = EQ
  compare Inf     _       = GT
  compare _       Inf     = LT
  compare (Fin x) (Fin y) = compare x y

-- | Define a semi-ring on the memory constraint. Note that the `-` sign does
--   not work like the normal extended natural numbers. Instead, it will always
--   truncate to zero for convenience.
instance Num MemConstr where
  fromInteger       = Fin . fromInteger
  Inf     + _       = Inf
  _       + Inf     = Inf
  (Fin x) + (Fin y) = Fin (x + y)
  Inf     - _       = Inf
  _       - Inf     = Fin 0
  (Fin x) - (Fin y) | x < y = Fin 0
                    | otherwise = Fin (x - y)
  Inf     * _       = Inf
  _       * Inf     = Inf
  (Fin x) * (Fin y) = Fin (x * y)
  abs x = x
  signum (Fin 0) = Fin 0
  signum x       = Fin 1

-- | Define a semigroup on the memory constraint which always picks the most
--   strict constraint of a pair.
instance Semigroup MemConstr where
  Inf <> other   = other
  other <> Inf   = other
  Fin x <> Fin y = Fin (min x y)

-- | Define the MemConstraint monoid by setting the 0-element to Inf.
instance Monoid MemConstr where
  mempty = Inf

-- | Removes the solutions that need more memory than the given constraint.
memFilter :: Solution a => MemConstr -> [a] -> [a]
memFilter Inf     = id
memFilter (Fin c) = filter $ (<c).mem

-- | Filters a list of solutions like `memFilter` and picks the one with
--   the lowest amount of fma-operations.
minFilt :: (Ord a, Solution a) => MemConstr -> [a] -> Maybe a
minFilt c = sminimum . memFilter c

--------------------------------------------------------------------------------
-- PRETTYPRINTING                                                             --
--------------------------------------------------------------------------------

-- Function dimensions

instance Pretty Dims where
  pretty (n :-> m) = pretty n <> "->" <> pretty m
  prettyList []             = "[]"
  prettyList [x]            = pretty x
  prettyList ((n :-> _):xs) = pretty n <> "->" <> prettyList xs

-- Function descriptions

prettyF :: F -> Doc a
prettyF f = pretty (n f) <> "-|" <> pretty (edges f) <> "|->" <> pretty (m f)

prettyFList :: [F] -> Doc a
prettyFList []     = "[]"
prettyFList [f]    = prettyF f
prettyFList (f:fs) = pretty (n f) <> "-|" <> pretty (edges f) <> "|->"
                                  <> prettyFList fs

instance Pretty F where
  pretty f = "F:" <+> prettyF f
  prettyList [] = "[]"
  prettyList fs = "F:" <+> prettyFList fs

-- Function ranges

instance Pretty FIx where
  pretty (i :...: j) = pretty i <> "..." <> pretty j

-- Bracketings

prettyBrak :: Brak -> Doc a
prettyBrak (I _) = "I"
prettyBrak (_ :>: r) = "(T" <+> prettyBrak r <> ")"
prettyBrak (l :<: _) = "(" <> prettyBrak l <+> "A)"
prettyBrak (l :*: r) = "(" <> prettyBrak l <+> "×" <+> prettyBrak r <> ")"

instance Pretty Brak where
  pretty b = nest 4 (vsep [prettyBrak b, prettyFma b, prettyMem b])

prettyFma, prettyMem :: Solution a => a -> Doc b
prettyFma s = "fma=" <+> pretty (fma s)
prettyMem s = "mem=" <+> pretty (mem s)

-- Mem Constraints

instance Pretty MemConstr where
  pretty Inf = "∞"
  pretty (Fin x) = pretty x
