{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
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
import qualified Data.Array.Accelerate.Algebra.Additive             as Additive
import qualified Data.Array.Accelerate.Algebra.IntegralDomain       as IntegralDomain ()
import qualified Data.Array.Accelerate.Algebra.ZeroTestable         as ZeroTestable

-- lol-accelerate
import Data.Array.Accelerate.Crypto.Lol.Types.Complex               ()
import Data.Array.Accelerate.Crypto.Lol.Types.ZqBasic               ()
import Data.Array.Accelerate.Crypto.Lol.CRTrans                     ()

import Crypto.Lol.Cyclotomic.Tensor.Accelerate.AT                   as AT
import Crypto.Lol.Cyclotomic.Tensor.Accelerate.Common               as Arr
import qualified Crypto.Lol.Cyclotomic.Tensor.Accelerate.CRT        as CRT
import qualified Crypto.Lol.Cyclotomic.Tensor.Accelerate.Dec        as Dec
import qualified Crypto.Lol.Cyclotomic.Tensor.Accelerate.Extension  as Ext
import qualified Crypto.Lol.Cyclotomic.Tensor.Accelerate.GL         as GL
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
                   , A.FromIntegral Int r
                   , A.Eq r                   -- entailEqT
                   , ZeroTestable.C (Exp r)   -- entalZTT, divGPow, divGDec
                   , Additive.C (Exp r)       -- entailModuleT
                   )

  entailIndexT  = tag $ Sub Dict
  entailEqT     = tag $ Sub Dict
  entailZTT     = tag $ Sub Dict
  entailNFDataT = tag $ Sub Dict
  entailRandomT = tag $ Sub Dict
  entailShowT   = tag $ Sub Dict
  entailModuleT = tag $ Sub Dict

  -- Make a raw value available for process in Accelerate
  constant x    = tag (A.constant x)

  -- Convert a scalar to a tensor in the powerful basis
  scalarPow     = AT . Pow.scalar

  -- 'l' converts from decoding-basis representation to powerful-basis
  -- representation; 'lInv' is its inverse.
  l             = AT.wrap GL.fL
  lInv          = AT.wrap GL.fLInv

  -- Multiply by @g@ in the powerful/decoding basis
  mulGPow       = AT.wrap GL.fGPow
  mulGDec       = AT.wrap GL.fGDec

  -- Divide by @g@ in the powerful/decoding basis. This operation is only
  -- possible when the input is evenly divisible by @g@.
  divGPow       = AT.wrapM GL.fGInvPow
  divGDec       = AT.wrapM GL.fGInvDec

  -- A tuple of all the operations relating to the CRT basis, in a single
  -- 'Maybe' value for safety. Clients should typically use the corresponding
  -- top-level functions instead.
  crtFuncs      = (,,,,) <$> ((AT.)   <$> CRT.scalar)
                         <*> (AT.wrap <$> CRT.mulGCRT)
                         <*> (AT.wrap <$> CRT.divGCRT)
                         <*> (AT.wrap <$> CRT.fCRT)
                         <*> (AT.wrap <$> CRT.fCRTInv)

  -- Sample from the "tweaked" Gaussian error distribution @t*D@ in the decoding
  -- basis, where @D@ has scaled variance @v@.
  -- tGaussianDec  =

  -- Given the coefficient tensor of @e@ with respect to the decoding basis of
  -- @R@, yield the (scaled) squared norm of @g_m \cdot e@ under the canonical
  -- embedding, namely:
  --
  --   @ \hat{m}^{-1} \cdot || \sigma ( g_m \cdot e ) ||^2 @
  -- gSqNormDec =

  -- The @twace@ linear transformation, which is the same in both the powerful
  -- and decoding bases.
  twacePowDec   = AT.wrap Ext.twacePowDec

  -- The @embed@ linear transformations, for the powerful and decoding bases
  embedPow      = AT.wrap Pow.embed
  embedDec      = AT.wrap Dec.embed

  -- A tuple of all the extension-related operations involving the CRT basis, in
  -- a single 'Maybe' value for safety. Clients should typically use the
  -- corresponding top-level functions instead.
  crtExtFuncs   = (,) <$> (AT.wrap <$> Ext.twaceCRT)
                      <*> (AT.wrap <$> CRT.embed)

  -- May a tensor in the powerful/decoding/CRT basis, representing an @O_m'@
  -- element, to a vector of tensors representing @O_m@ elements in the same
  -- kind of basis.
  coeffs        = AT.wrapM Ext.coeffs

  -- The powerful extension basis w.r.t. the powerful basis
  powBasisPow   = (AT <$>) <$> Pow.powBasis

  -- A list of tensors representing the mod-@p@ CRT set of the extension
  crtSetDec     = (AT <$>) <$> Ext.crtSetDec

  -- Auxiliary
  fmapT f       = AT.wrap  $ Arr.wrap  (A.map f)
  zipWithT f    = AT.wrap2 $ Arr.wrap2 (A.zipWith f)

  unzipT xs     = (AT (Arr ls), AT (Arr rs))
    where
      AT (Arr xs') = toAT xs
      (ls,rs)      = A.unzip xs'


-- Mirroring the behaviour of numeric-prelude.fromIntegral which is driven by
-- Ring.fromInteger and the product instance defined in LatticePrelude.
--
instance (FromIntegral a b, FromIntegral a c, Elt b, Elt c) => FromIntegral a (b,c) where
  fromIntegral x = A.lift (A.fromIntegral x, A.fromIntegral x)

