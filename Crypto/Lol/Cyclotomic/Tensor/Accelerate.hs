{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}
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

  AT,

) where

-- accelerate
import Data.Array.Accelerate                                        as A hiding ((++), (==), lift)
import Data.Array.Accelerate.Data.BigInt
import qualified Data.Array.Accelerate                              as A

-- numeric-prelude-accelerate
import qualified Data.Array.Accelerate.Algebra.Additive             as Additive
import qualified Data.Array.Accelerate.Algebra.IntegralDomain       as IntegralDomain
import qualified Data.Array.Accelerate.Algebra.RealRing             as RealRing ()
import qualified Data.Array.Accelerate.Algebra.Ring                 as Ring
import qualified Data.Array.Accelerate.Algebra.Transcendental       as Transcendental ()
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
import Crypto.Lol.CRTrans
import Crypto.Lol.Cyclotomic.Tensor
import Crypto.Lol.Gadget
import Crypto.Lol.Prelude                                           as P hiding ( FromIntegral )
import qualified Crypto.Lol.Prelude                                 as P
import Crypto.Lol.Types.IZipVector
import Crypto.Lol.Types.Proto

-- other libraries
import Control.Applicative
import Data.Constraint
import Data.Maybe
import qualified Prelude


-- | Accelerate-backed Tensor instance
--
instance Tensor AT where
  type TElt AT r = ( Elt r
                   , CRTIndex (Exp r) ~ Exp Int
                   , A.FromIntegral Int r
                   , A.Eq r                   -- entailEqT
                   , ZeroTestable.C (Exp r)   -- entalZTT, divGPow, divGDec
                   , Additive.C (Exp r)       -- entailModuleT
                   , Ring.C (Exp r)           -- entailModuleT
                   )
  type TRep AT r = Exp r

  entailIndexT  = tag $ Sub Dict
  entailEqT     = tag $ Sub Dict
  entailZTT     = tag $ Sub Dict
  entailNFDataT = tag $ Sub Dict
  entailRandomT = tag $ Sub Dict
  entailShowT   = tag $ Sub Dict
  entailModuleT = tag $ Sub Dict

  rep = return . A.constant

  -- Make a raw value available for process in Accelerate
  --constant x    = tag (A.constant x)

  -- Convert a scalar to a tensor in the powerful basis
  scalarPow     = AT . Pow.scalar . A.constant

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
  crtFuncs      = (,,,,) <$> ((AT.)   <$> scalarCRT')
                         <*> (AT.wrap <$> CRT.mulGCRT)
                         <*> (AT.wrap <$> CRT.divGCRT)
                         <*> (AT.wrap <$> CRT.fCRT)
                         <*> (AT.wrap <$> CRT.fCRTInv)

  -- Sample from the "tweaked" Gaussian error distribution @t*D@ in the decoding
  -- basis, where @D@ has scaled variance @v@.
  tGaussianDec  = fmap AT . Dec.tGaussian

  -- Given the coefficient tensor of @e@ with respect to the decoding basis of
  -- @R@, yield the (scaled) squared norm of @g_m \cdot e@ under the canonical
  -- embedding, namely:
  --
  --   @ \hat{m}^{ -1 } \cdot || \sigma ( g_m \cdot e ) ||^2 @
  gSqNormDec    = AT.unwrap Dec.gSqNorm

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


scalarCRT' :: (CRTrans mon (Exp r), Fact m, Elt r) => mon (r -> Arr m r)
scalarCRT' = do
  f <- CRT.scalar
  return $ f . A.constant


-- missing instances
-- -----------------

type instance LiftOf (Exp (a,b)) = Exp Int64  -- ~ Integer

instance (FromIntegral a b, FromIntegral a c, Elt b, Elt c) => FromIntegral a (b,c) where
  fromIntegral x = A.lift (A.fromIntegral x, A.fromIntegral x)

instance (Reduce a (Exp b), Reduce a (Exp c), Elt b, Elt c) => Reduce a (Exp (b,c)) where
  reduce x =
    let b = reduce x :: Exp b
        c = reduce x :: Exp c
    in
    A.lift (b,c)

instance ( Mod a, ToInteger (ModRep a), Lift' (Exp a), Reduce (Exp Int64) (Exp a), LiftOf (Exp a) ~ Exp Int64, Elt a
         , Mod b, ToInteger (ModRep b), Lift' (Exp b), Reduce (Exp Int64) (Exp b), LiftOf (Exp b) ~ Exp Int64, Elt b)
    => Lift' (Exp (a,b)) where
  lift t =
    let moda    = toInteger $ proxy modulus (Proxy::Proxy a)
        modb    = toInteger $ proxy modulus (Proxy::Proxy b)
        ainv    = fromMaybe (error "Lift' (a,b): moduli not coprime") $ moda `modinv` modb
        q       = moda P.* modb
        --
        (a,b)   = A.unlift t :: (Exp a, Exp b)
        lifta   = A.fromIntegral (P.lift a) :: Exp Int128
        liftb   = A.fromIntegral (P.lift b) :: Exp Int128
        q'      = constant (P.fromInteger q)
        moda'   = constant (P.fromInteger moda)
        ainv'   = constant (P.fromInteger ainv)
        -- This intermediate computation must be done with 128-bits, since
        -- multiplying by the moduli can overflow Int64
        (_,r)   = (moda' P.* (liftb P.- lifta) P.* ainv' P.+ lifta) `divModCent` q'
    in
    A.fromIntegral r -- put in [-q/2, q/2)

-- instance (Mod (Exp a), Mod (Exp b), RealRing (ModRep (Exp a)), RealRing (ModRep (Exp b)), Elt a, Elt b)
--     => Mod (Exp (a,b)) where
--   type ModRep (Exp (a,b)) = Exp Int64 -- ~ Integer
--   modulus = tag $ P.truncate (proxy modulus (Proxy::Proxy (Exp a))) P.*
--                   P.truncate (proxy modulus (Proxy::Proxy (Exp b)))

instance (Mod a, Mod b, ToInteger (ModRep a), ToInteger (ModRep b), Additive (Exp a), Additive (Exp b), Elt a, Elt b)
    => Mod (Exp (a,b)) where
  type ModRep (Exp (a,b)) = Exp Int64 -- ~ Integer
  modulus = constant . P.fromInteger <$> retag (modulus :: Tagged (a,b) (ModRep (a,b)))

instance (Decompose gad (Exp a), Decompose gad (Exp b), DecompOf (Exp a) ~ DecompOf (Exp b), Elt a, Elt b)
    => Decompose gad (Exp (a,b)) where
  type DecompOf (Exp (a,b)) = DecompOf (Exp a)
  decompose x =
    let (a,b) = unlift x :: (Exp a, Exp b)
    in  (P.++) <$> decompose a <*> decompose b

instance (Gadget gad (Exp a), Gadget gad (Exp b), Elt a, Elt b) => Gadget gad (Exp (a, b)) where
  gadget = (P.++) <$> (P.map (\x -> A.lift ((x,zero) :: (Exp a, Exp b))) <$> gadget)
                  <*> (P.map (\x -> A.lift ((zero,x) :: (Exp a, Exp b))) <$> gadget)

instance (Mod (Exp b), Field (Exp a), P.Lift (Exp b) (ModRep (Exp b)), Reduce (LiftOf (Exp b)) (Exp a), Elt a, Elt b)
    => Rescale (Exp (a,b)) (Exp a) where
  rescale t =
    let
        q2val   = proxy modulus (Proxy::Proxy (Exp b))
        q2inv   = P.recip (reduce q2val)
        (x1,x2) = A.unlift t  :: (Exp a, Exp b)
    in
    q2inv P.* (x1 P.- reduce (P.lift x2))

instance (Mod (Exp a), Field (Exp b), P.Lift (Exp a) (ModRep (Exp a)), Reduce (LiftOf (Exp a)) (Exp b), Elt a, Elt b)
    => Rescale (Exp (a,b)) (Exp b) where
  rescale t =
    let
        q1val   = proxy modulus (Proxy::Proxy (Exp a))
        q1inv   = P.recip (reduce q1val)
        (x1,x2) = A.unlift t  :: (Exp a, Exp b)
    in
    q1inv P.* (x2 P.- reduce (P.lift x1))

instance (Ring (Exp a), Mod (Exp b), Reduce (ModRep (Exp b)) (Exp a), Elt a, Elt b)
    => Rescale (Exp a) (Exp (a,b)) where
  rescale x = let q2val = reduce $ proxy modulus (Proxy::Proxy (Exp b))
              in A.lift ( q2val P.* x :: Exp a
                        , zero        :: Exp b)

instance (Ring (Exp b), Mod (Exp a), Reduce (ModRep (Exp a)) (Exp b), Elt a, Elt b)
    => Rescale (Exp b) (Exp (a,b)) where
  rescale x = let q1val = reduce $ proxy modulus (Proxy::Proxy (Exp a))
              in  A.lift ( zero        :: Exp a
                         , q1val P.* x :: Exp b)

instance P.FromIntegral (Exp Int64) (Exp Double) where
  fromIntegral' = A.fromIntegral

instance P.Round (Exp Double) (Exp Int64) where
  round x =
    let n = toFloating (A.truncate x :: Exp Int64)
        f = x A.- n
    in
    A.truncate (n A.+ 2 A.* f)

  roundMult i r =
    i A.== one ? ( P.round r
                 , P.round (r A./ A.fromIntegral i) A.* i )

instance (Protoable (IZipVector m r), Fact m, Elt r) => Protoable (AT m r) where
  type ProtoType (AT m r) = ProtoType (IZipVector m r)

  toProto x@AT{} = toProto $ toZV x
  toProto (ZV x) = toProto x

  fromProto x = toAT <$> ZV <$> fromProto x

instance Ring.C Int128 where
  (*)         = (Prelude.*)
  fromInteger = Prelude.fromInteger

instance Additive.C Int128 where
  zero   = 0
  (+)    = (Prelude.+)
  (-)    = (Prelude.-)
  negate = Prelude.negate

instance Ring.C (Exp Int128) where
  (*)         = (Prelude.*)
  fromInteger = Prelude.fromInteger

instance Additive.C (Exp Int128) where
  zero   = 0
  (+)    = (Prelude.+)
  (-)    = (Prelude.-)
  negate = Prelude.negate

instance IntegralDomain.C (Exp Int128) where
  divMod = Prelude.divMod

