{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
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

module Crypto.Lol.Cyclotomic.Tensor.Accelerate.Pow (

  scalar, embed

) where

-- accelerate & friends
import Data.Array.Accelerate                                        as A
import Data.Array.Accelerate.IO                                     as A

import qualified Data.Array.Accelerate.Algebra.Additive             as Additive ()

-- lol/lol-accelerate
import Crypto.Lol.Cyclotomic.Tensor.Accelerate.Common
import Crypto.Lol.LatticePrelude
import qualified Crypto.Lol.Cyclotomic.Tensor                       as T

-- other libraries
import qualified Data.Vector.Unboxed                                as U


-- | Embeds a scalar into a powerful-basis representation, tagged by the
-- cyclotomic index.
--
scalar :: forall m r. (Fact m, Additive (Exp r), Elt r) => Exp r -> Arr m r
scalar r =
  let
      n  = proxy totientFact (Proxy :: Proxy m)
      sh = constant (Z :. n)

      f :: Exp DIM1 -> Exp r
      f (unindex1 -> i) = i ==* 0 ? ( r , zero )
  in
  Arr $ generate sh f


-- | Embeds an array in the powerful basis of the m`th cyclotomic ring, to an
-- array in the powerful basis of the m'`th cyclotomic ring, when @m | m'@
--
embed :: forall m m' r. (m `Divides` m', Additive (Exp r), Elt r)
      => Arr m  r
      -> Arr m' r
embed (Arr arr) =
  let
      indices = proxy baseIndicesPow (Proxy::Proxy '(m,m'))
      f jix   =
        let (j,ix) = A.unlift jix
        in  j ==* zero ? ( arr A.!! ix , zero )
  in
  Arr $ A.map f indices


-- Reindexing arrays
-- -----------------
--
-- TODO: Make sure these really are memoised.
--

-- NOTE:
--  * unzip of unboxed vector is O(1)
--  * conversion to storable vectors is O(n)
--  * conversion from storable to Accelerate is O(1)
--
baseIndicesPow
    :: forall m m'. (m `Divides` m')
    => Tagged '(m,m') (Acc (Vector (Int,Int)))
baseIndicesPow = do
  (ix,iy) <- U.unzip <$> T.baseIndicesPow
  let ix'  = U.convert ix
      iy'  = U.convert iy
  --
  return . A.use $! A.fromVectors (Z :. U.length ix) (((), ix'), iy')

