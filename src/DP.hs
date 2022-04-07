{-# LANGUAGE OverloadedStrings
           , OverloadedLabels
           , NoMonomorphismRestriction
           , DuplicateRecordFields
           , DeriveGeneric
           , FlexibleInstances
           , FlexibleContexts
           , TupleSections
           , RankNTypes #-}

module DP where

import Control.Lens hiding ((...))
import Control.Comonad.Env
import Control.Comonad.Store
import Control.Comonad.Identity
import Control.Comonad.Traced
import Data.Generics.Labels
import Data.Generics.Product
import Data.Maybe
import Data.Default
import GHC.Generics hiding (to, (:*:))
import GHC.Arr as A
import Prettyprinter
import Common
import Types

--------------------------------------------------------------------------------
-- DYNAMIC PROGRAM INPUT QUERY                                                --
--------------------------------------------------------------------------------

-- | The input for an dynamic program.
--   These values are mainly used to initialize the algorithm and the
--   accomponied datastructures of the fix-point calculations.
data Input = Input { fnComp :: [F] -- ^ The function composition.
                   , memConstr :: MemConstr -- ^ The memory constraints.
                   } deriving (Show, Read, Generic)

-- | Smart constructor of the input.
input :: (FuncComp a, MemConstraint b) => a -> b -> Input
input fs memc = Input { fnComp = toFnComp fs
                      , memConstr = memConstraint memc }

instance Default Input where
  def = Input [] Inf

--------------------------------------------------------------------------------
-- DYNAMIC PROGRAM SUB-PROBLEM QUERY                                          --
--------------------------------------------------------------------------------

-- | Datatype that encodes a query to a subproblem of a dynamic program.
data Query = Query { index :: FIx -- ^ The index of the sub-func composition.
                   , memConstr :: MemConstr -- ^ The sub-memory constraint.
                   } deriving (Show, Read, Generic)

instance Default Query where
  def = Query def Inf


-- Smart constructor for a query.
query :: (Ranged a, MemConstraint b) => a -> b -> Query
query ix memc = Query { index = whole ix, memConstr = memConstraint memc }

-- | Converts an input to a query.
toQuery :: Input -> Query
toQuery i = def & (#index . _sizeR) .~ (i ^. #fnComp . to length)
                & #memConstr .~ (i ^. #memConstr)

--------------------------------------------------------------------------------
-- COMONADS                                                                   --
--------------------------------------------------------------------------------

-- STORE TRANSFORMER

-- | The main comonad that abstracts queries to a dynamic program.
--   It has the `Input` as it's environment and `Query` as it's store position.
type Queryable = StoreT Query (Env Input)

-- | The result type of a dyanamic program.
type DPResult = Maybe Brak

-- | A store for the results of a dynamic program.
type DPStore = Queryable DPResult

-- | A dynamic program, which is a function from the result values of a
--   dynamic program to one result.
type DP = DPStore -> DPResult

-- | Converts a dynamic program to an queryable dynamic program.
toQueryable :: Input -> DP -> Queryable DP
toQueryable i f = StoreT (env i $ const f) (toQuery i)

-- | Converts an input to the queryable dynamic program that finds the best
--   solution.
toDP :: Input -> Queryable DP
toDP i = toQueryable i dp

-- | Creates a new queryable dynamic program that finds the best solution.
(!<!) :: (FuncComp a, MemConstraint b) => a -> b -> Queryable DP
fs !<! c = toDP (input fs c)

--------------------------------------------------------------------------------
-- COMONAD LENS HELPERS                                                       --
--------------------------------------------------------------------------------

-- | Same as the usual `ask` of an Env comonad, but with `getter` optics.
lAsk :: ComonadEnv a w => Getter a b -> w c -> b
lAsk l = asks (view l)

-- | Same as the usual `pos` of a Store comonad, but with `getter` optics.
lPos :: ComonadStore a w => Getter a b -> w c -> b
lPos l q = pos q ^. l

-- | Same as the usual `seek` of a Store comonad, but with `setter` optics.
lSeek :: ComonadStore a w => Setter' a b -> b -> w c -> w c
lSeek l v = seeks (l .~ v)

-- | Same as the usual `seeks` of a Store comonad, but with `lens` optics.
lSeeks :: ComonadStore a w => Lens' a b -> (b -> b) -> w c -> w c
lSeeks l f = seeks (l %~ f)

--------------------------------------------------------------------------------
-- HELPER CO-KLEISLI ARROWS                                                   --
--------------------------------------------------------------------------------

-- OVER THE INPUT

-- | Gives the top function composition of the query.
inputFnComp :: Queryable a -> [F]
inputFnComp = lAsk #fnComp

-- | Whether the input query is the empty query.
inputEmpty :: Queryable a -> Bool
inputEmpty = null . inputFnComp

-- | Gives the initial memory constraint of the query.
inputMemConstraint :: Queryable a -> MemConstr
inputMemConstraint = lAsk #memConstr

-- | Gives the length of the initial function composition of the query.
inputLength :: Queryable a -> Int
inputLength = length . inputFnComp

-- | Gives the initial position of the query.
inputPos :: Queryable a -> Query
inputPos = asks toQuery

-- | Gives the initial position index of the query.
inputIx :: Queryable a -> FIx
inputIx = view #index . inputPos

-- | Gives the initial index range of the query.
inputIxRange :: Queryable a -> (FIx, FIx)
inputIxRange = (1...1,) . inputIx

-- OVER THE SUB-PROBLEM POSITION

-- | Gives the current sub-problem index position of the query.
qIx :: Queryable a -> FIx
qIx = lPos #index

-- | Wheter the current sub-problem index position of the query is the empty
--   subproblem.
qIsEmpty :: Queryable a -> Bool
qIsEmpty = lPos (#index . empty)

-- | Gives the memory constraint of current position of the query.
qMemConstr :: Queryable a -> MemConstr
qMemConstr = lPos #memConstr

-- | Gives the function composition of the current position of the query.
qFnComp :: Queryable a -> [F]
qFnComp q = slice (lPos #index q) (lAsk #fnComp q)

-- | Gives the dimensions of the function composition of the current
--   position of the query.
qN, qM, qEdges :: Queryable a -> Nat
qN = n . qFnComp
qM = m . qFnComp
qEdges = edges . qFnComp

--------------------------------------------------------------------------------
-- QUERY TRANSFORMERS (Mainly for convenience in the REPL)                    --
--------------------------------------------------------------------------------

-- | Sets the memory constraint of the sub-problem position.
seekMemConstr :: MemConstraint a => a -> Queryable b -> Queryable b
seekMemConstr c = lSeek #memConstr (memConstraint c)

-- | Changes the sub-problem index position of a query.
seekIx :: FIx -> Queryable a -> Queryable a
seekIx = lSeek #index

-- | Decreases the memory constraint by the provided amount.
addCost :: Nat -> Queryable a -> Queryable a
addCost n = lSeeks #memConstr (subtract $ Fin n)

--------------------------------------------------------------------------------
-- FIX-POINTS USING A RESULTS TABLE                                           --
--------------------------------------------------------------------------------

-- TABLE-FIX IMPLEMENTATION

-- | Find the fixpoint of an algorithm query. It uses an array of `MemLists`
--   to store the results that are reusable. It is a lot faster than the usual
--   `wfix`-function.
tfix :: Queryable DP -> Maybe Brak
tfix q = prep q
  where table  = array bounds ((\i -> (i,l i)) <$> range bounds)
        bounds = inputIxRange q
        prep x = view #solution <$> findListEntry (qMemConstr x) (table ! qIx x)
        l i    = memList (seekIx i q =>> prep =>> extract q)

-- MEMORY LIST ENTRIES

-- | An entry of a MemoryList. It stores the intermediate solutions with
--   the memory constraint for which that solution was the best.
data MemListEntry = MemListEntry { memConstr :: MemConstr
                                 , solution :: Brak
                                 } deriving ( Show, Read, Eq, Generic)

instance FuncDims MemListEntry where toDims   = toDims   . view #solution
instance FuncDesc MemListEntry where toF      = toF      . view #solution
instance FuncComp MemListEntry where toFnComp = toFnComp . view #solution
instance Solution MemListEntry where toBrak   = toBrak   . view #solution

-- MEMORY LIST

-- | A list of best solutions for one sub-problem ordered by memory constraint
--   in decreasing order.
newtype MemList = MemList { list :: [MemListEntry]
                          } deriving (Show, Eq, Generic)

-- | Gets an entry from a MemList within the memory constraint.
findListEntry :: MemConstr -> MemList -> Maybe MemListEntry
findListEntry Inf     (MemList xs) = safeHead xs
findListEntry (Fin c) (MemList xs) = go xs
  where go [] = Nothing
        go (x:xs) | mem x < c = Just x
                  | otherwise = go xs

-- | Smart constructor that generates a solution list using the values in a
--   dynamic program store.
memList :: DPStore -> MemList
memList = MemList . go
  where go q = do
              e <- maybeToList $ entry q
              e:go (next e q)
        next e q = lSeek #memConstr (nextMem e) q
        entry q  = MemListEntry (lPos #memConstr q) <$> extract q

--------------------------------------------------------------------------------
-- DYNAMIC PROGAM IMPLEMENTATIONS                                             --
--------------------------------------------------------------------------------

-- | Dynamic program that finds the best homogenious bracketing within the
--   memory constraint of the query position.
dpHom :: DP
dpHom q | qIsEmpty q = Nothing -- Return nothing when the query is empty.
        | otherwise  = minFilt c [adj fs, tngt fs]
  where fs = qFnComp q      -- The top function composition.
        c  = qMemConstr q   -- The memory constraint of the query.

-- | The main dynamic program that finds the best solution (lowest fma)
--   withing the memory contraint of the current query position.
--
--   It considers every possible bracketing in it's search.
dp :: DP
dp q | len <= 1  = dpHom q -- Is always an homogenious function.
     | otherwise = sminimum subResults -- Find the best solution.
  where
    ix  = qIx q           -- The top index of the query.
    len = size ix         -- The length of the result.
    -------------------- SPLITTING INTO SUB-PROBLEMS ---------------------
    subResults = do               -- [Uses the `List`-monad].
      (rIx, lIx) <- splits' ix    -- For each split into subproblems.
      let ql = seekIx lIx q       -- The subquery for the left subproblem.
      let qr = seekIx rIx q       -- The subquery for the right subproblem.
      mode <- [adjMode, tngtMode, preaccMode] -- For each accumulation mode.
      maybeToList (mode ql qr)    -- Discard invalid results.
    ------------------------- TANGENT MODE -------------------------------
    tngtMode ql qr = do                -- [Uses the `Maybe`-monad].
      let lhs  = qFnComp ql            -- Query the lhs func-composition.
      rhs <- extract qr                -- Query the best rhs bracketing.
      return (tngtAfter lhs rhs)       -- Tangent `lhs` after `rhs`.
    ------------------------- ADJOINT MODE -------------------------------
    adjMode ql qr  = do                -- [Uses the `Maybe`-monad].
      let rhs  = qFnComp qr            -- Query the rhs func-composition.
      let memc = edges rhs             -- Computes the added memory cost.
      lhs <- extract (addCost memc ql) -- Query the best lhs bracketing.
      return (adjAfter rhs lhs)        -- Adjoint `rhs` after `lhs`.
    ---------------------- PREACCUMULATION MODE --------------------------
    preaccMode ql qr = do              -- [Uses the `Maybe`-monad].
      let memc   = qM ql * qN qr       -- Computes the added memory cost.
      lhs <- extract (addCost memc ql) -- Query the best lhs bracketing.
      rhs <- extract qr                -- Query the best rhs bracketing.
      return (lhs :*: rhs)             -- Matrix mutiply `rhs` with `lhs`.

--------------------------------------------------------------------------------
-- PRETTY PRINTING                                                            --
--------------------------------------------------------------------------------

instance Pretty Input where
  pretty x = "Input:" <+> pretty (x ^. #fnComp)
                      <+> "<" <+> pretty (x ^. #memConstr)

instance Pretty Query where
  pretty x = "Query:" <+> pretty (x ^. #index)
                      <+> "<" <+> pretty (x ^. #memConstr)

instance Pretty a => Pretty (Queryable a) where
  pretty x = nest 4 $ vsep [ "------ QUERYABLE ------"
                           , pretty (ask x)
                           , pretty (pos x)
                           , nest 4 $ "Value:" <+> line <> pretty (extract x)
                           ]

instance Pretty MemListEntry where
  pretty x = nest 4 $ vsep rs
    where rs = prettyBrak (x ^. #solution):vals
          memRange = "mem=" <+> pretty (mem x)
                            <> "..<" <> pretty (x ^. #memConstr)
          vals = punctuate comma [ memRange, prettyFma x ]

instance Pretty MemList where
  pretty xs = vsep $ pretty <$> xs ^. #list
