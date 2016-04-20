{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}
-- |
-- Module      : Crypto.Lol.Cyclotomic.Tensor.Accelerate.Pow
-- Copyright   : [2016] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Functions to support operations in the powerful basis
--

module Crypto.Lol.Cyclotomic.Tensor.Accelerate.Pow
  where

import Data.Array.Accelerate                                        as A
import qualified Data.Array.Accelerate.Algebra.Additive             as Additive ()

import Crypto.Lol.Cyclotomic.Tensor.Accelerate.Common
import Crypto.Lol.LatticePrelude


-- | Embeds a scalar into a powerful-basis representation, tagged by the
-- cyclotomic index.
--
scalar :: forall m r. (Fact m, Additive (Exp r), Elt r) => r -> Arr m r
scalar (constant -> r) =
  let
      n  = proxy totientFact (Proxy :: Proxy m)
      sh = constant (Z :. n)

      f :: Exp DIM1 -> Exp r
      f (unindex1 -> i) = i ==* 0 ? ( r , zero )
  in
  Arr $ generate sh f

