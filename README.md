# Implementation of the Dynamic Program in Haskell

## Project Layout

This project only contains 4 modules:

 - `Common`: Some common, simple functions used the application
             (mostly `Maybe`-safe implementations of list functions.)
 - `Types`: Implementation of the types needed to model the bracketing problem
            in _Haskell_ and some convenience functions to manipulate them.
 - `DP`: The implementation of the dynamic program, including the fix-point
         operator that uses tables to speed up.
 - `Lib`: A module that re-exposes all the modules needed to experiment with
          this implementation in `ghci`, as well as some example inputs.
          (It is the only module that you need to import.)

## Installation

Clone the repository and build with *cabal* (using `cabal build`.)

## Usage

The best way to experiment with this implementation is from within the
`ghci`-repl. To start, use the following command:

```{sh}
cabal repl
```

### Building inputs

The easiest way to build a function composition that can be used as an input
is to use the helper function:

```{Haskell}
zipF :: [Nat] -> [Nat] -> [F]
```
where the first list should contain the dimensions and the second list the
amount of edges in the execution-graph of that function.

```{Haskell}
exampleA = zipF [8,4,1] [16,16]
```

You might also use list-comprehensions and the normal constructors to generate
more complex inputs like:

```{Haskell}
exampleB = [F ((2^x) :-> (2^(x-1))) (2^(x+2)) | x <- [10,9..1]]
```

To convert a function compositions (List of functions, type=`[F]`) to the input
of a dynamic program, you can use the infix operator `!<!`

```{Haskell}
(!<!) :: (FuncComp a, MemConstraint b) => a -> b -> Queryable DP
```

where the _lhs_ is the function composition and the _rhs_ is the memory
constraint.

You can give the value of the memory constraint `memc` by the `Fin memc`, to
only allow bracketing that use less than `memc` memory. You can also set this
value to `Inf` if you want a dynamic program that has no memory constraints:

```{Haskell}
dpA = exampleA !<! Inf      -- No memory constraints.
dpB = exampleB !<! Fin 2000 -- Only solutions that use less than 2000 memory.
```

### Finding fix-points (Solving the Dynamic Program.)

To find a fix point, you can use the standard function `wfix` from the `comonad`
library:

```{Haskell}
wfix $ exampleA !<! Inf
```

However, this is extremely slow for big function compositions as it re-evaluates
almost every sub-problem multiple times.

We also implemented a faster function called `tfix` that uses a table of linked
lists. This implementation evaluates a sub-problem at most 1 on time, and is
therefore a lot faster than the more general one.

```{Haskell}
tfix $ exampleB !<! Fin 2000
```

When used like this, the results of `wfix` and `tfix` are the same.

### Exploring the sub-problems.

The dynamic program is implemented using comonads (see the `Queryable` comonad
in the `DP`-module). You can use this to your advantage if you want to see the
results of the sub-problems by extending the dynamic program using one of the
fix-point operators:

```{Haskell}
let x = exampleB !<! Fin 2000 =>> tfix :: Queryable DPResult
```

Besides the standard operations of a `Store`-comonad (a co-`State` monad)
operations found in the `comonad` library, you can also use one of the following
derived functions that are a bit more convenient to use:

```{Haskell}
-- | Sets the memory constraint of the sub-problem position.
seekMemConstr :: MemConstraint a => a -> Queryable b -> Queryable b

-- | Changes the sub-problem index position of a query.
seekIx :: FIx -> Queryable a -> Queryable a

-- | Decreases the memory constraint by the provided amount.
addCost :: Nat -> Queryable a -> Queryable a
```

Afterwards, you can use `extract` to retrieve the value from the comonad.

The following example gives the sub-result of the sub-problem _F_4 * F_3 * F_2_
with the memory constraint reduces by `1400`:

```{Haskell}
-- Making an extended queryable object using the `tfix`-operator.
let x = exampleB !<! Fin 2000 =>> tfix

-- Changing the `Store`-monad using the convenient functions.
let x' = (addCost 1400 . seekIx 2...4) x

-- Retrieve the sub-result using the normal `extract` function.
extract x'
```

**IMPORTANT:** Use the `tfix` operator, as `wfix` needs to re-evaluate
all subproblems every time you use `extract`.

### Pretty Printing

For most of the `Show`-able types, we also defined a pretty-print function to
make the results more readable. You can use this feature by prefixing a line
in `ghci` with `pretty $`.

```{Haskell}
-- Pretty-prints the result of `exampleA`, including the positional data from
-- the `Queryable` comonad.
pretty $ exampleA !<! Inf =>> wfix

-- Pretty-prints the sub-result where the memory-constraint is set to 20:
pretty $ seekMemConstr (Fin 20) $ exampleA !<! Inf =>> tfix
```

Both the `DP`- and `Types`-modules have their pretty-print instances defined
at the bottom of the source-file.

## Remarks

This is a simplified version that is well-documented and easier to understand.
We used another, more complex implementation of the dynamic program which you
can find at [my fork of the IHaskell project](https://github.com/roelhem/my-ihaskell/tree/main/custom/math-tools).
