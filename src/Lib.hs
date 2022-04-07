module Lib ( module Lib
           , module Types
           , module Common
           , module DP
           , module Prettyprinter
           , module Control.Lens
           , module Control.Comonad
           , module Control.Comonad.Env
           , module Control.Comonad.Store
           , module Data.Generics.Labels
           , module Data.Generics.Product
           , module Data.List
           , module Data.List.Lens
           , module Data.Maybe
           , module Data.Bool
           , module Data.Default
           , module Numeric.Natural
           , module GHC.OverloadedLabels
           ) where

import Types
import Common
import DP

-- FORWARDING
import Numeric.Natural
import Prettyprinter hiding (group)
import Control.Lens hiding ((...))
import Control.Comonad
import Control.Comonad.Env hiding (lower)
import Control.Comonad.Store hiding (lower)
import Data.Generics.Labels
import Data.Generics.Product hiding (list)
import Data.List hiding (uncons)
import Data.List.Lens
import Data.Maybe
import Data.Bool
import Data.Default
import GHC.OverloadedLabels

-- Example function compositions.
exampleA = zipF [8,4,1] [16,16]
exampleB = [F ((2^x) :-> (2^(x-1))) (2^(x+2)) | x <- [10,9..1]]
