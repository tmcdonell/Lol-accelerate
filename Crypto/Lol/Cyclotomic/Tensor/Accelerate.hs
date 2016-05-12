{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Crypto.Lol.Cyclotomic.Tensor.Accelerate
-- Copyright   : [2016] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Tensor instance backed by Accelerate
--

module Crypto.Lol.Cyclotomic.Tensor.Accelerate (

  Tensor(..), AT, TRep,

) where

-- accelerate
import Data.Array.Accelerate                                        as A

-- numeric-prelude-accelerate
import qualified Data.Array.Accelerate.Algebra.Additive             as Additive ()
import qualified Data.Array.Accelerate.Algebra.IntegralDomain       as IntegralDomain ()
import qualified Data.Array.Accelerate.Algebra.ZeroTestable         as ZeroTestable

-- lol-accelerate
import Data.Array.Accelerate.Crypto.Lol.Types.Complex               ()
import Data.Array.Accelerate.Crypto.Lol.Types.ZqBasic               ()
import Data.Array.Accelerate.Crypto.Lol.CRTrans                     ()

import Crypto.Lol.Cyclotomic.Tensor.Accelerate.AT
import Crypto.Lol.Cyclotomic.Tensor.Accelerate.Common
import qualified Crypto.Lol.Cyclotomic.Tensor.Accelerate.GL         as GL
import qualified Crypto.Lol.Cyclotomic.Tensor.Accelerate.CRT        as CRT
import qualified Crypto.Lol.Cyclotomic.Tensor.Accelerate.Pow        as Pow

-- lol
import Crypto.Lol.Cyclotomic.Tensor
import Crypto.Lol.LatticePrelude                                    as P

-- other libraries
import Control.Applicative
import Data.Constraint


-- | Accelerate-backed Tensor instance
--
instance Tensor AT where
  type TElt AT r = ( Elt r
                   , A.Eq r                   -- for entailEqT
                   , ZeroTestable.C (Exp r)   -- for divGPow, divGDec
                   )

  entailIndexT  = tag $ Sub Dict
  entailEqT     = tag $ Sub Dict
  -- entailZTT     = tag $ Sub Dict
  -- entailNFDataT = tag $ Sub Dict
  entailRandomT = tag $ Sub Dict
  entailShowT   = tag $ Sub Dict
  -- entailModuleT = tag $ Sub Dict

  -- Make a raw value available for process in Accelerate
  constant x    = tag (A.constant x)

  -- Convert a scalar to a tensor in the powerful basis
  scalarPow     = AT . Pow.scalar

  -- 'l' converts from decoding-basis representation to powerful-basis
  -- representation; 'lInv' is its inverse.
  l             = wrap GL.fL
  lInv          = wrap GL.fLInv

  -- Multiply by @g@ in the powerful/decoding basis
  mulGPow       = wrap GL.fGPow
  mulGDec       = wrap GL.fGDec

  -- Divide by @g@ in the powerful/decoding basis. This operation is only
  -- possible when the input is evenly divisible by @g@.
  divGPow       = wrapM GL.fGInvPow
  divGDec       = wrapM GL.fGInvDec

  -- A tuple of all the operations relating to the CRT basis, in a single
  -- 'Maybe' value for safety. Clients should typically class the corresponding
  -- top-level functions instead.
  crtFuncs      = (,,,,) <$> ((AT.) <$> CRT.scalar)
                         <*> (wrap  <$> CRT.mulGCRT)
                         <*> (wrap  <$> CRT.divGCRT)
                         <*> (wrap  <$> CRT.fCRT)
                         <*> (wrap  <$> CRT.fCRTInv)

  fmapT f       = wrap (Arr . A.map f . unArr)

  zipWithT f xs ys = AT $ Arr (A.zipWith f xs' ys')
    where
      AT (Arr xs') = toAT xs  -- this style rather than ViewPatterns to...
      AT (Arr ys') = toAT ys  -- ...avoid non-exhaustive pattern warnings

  unzipT xs = (AT (Arr ls), AT (Arr rs))
    where
      AT (Arr xs') = toAT xs
      (ls,rs)      = A.unzip xs'

