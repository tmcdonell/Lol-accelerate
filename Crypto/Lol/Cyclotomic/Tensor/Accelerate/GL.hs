{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
-- |
-- Module      : Crypto.Lol.Cyclotomic.Tensor.Accelerate.GL
-- Copyright   : [2016] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- The @G@ and @L@ transforms for Accelerate arrays
--

module Crypto.Lol.Cyclotomic.Tensor.Accelerate.GL (

  fL, fLInv,
  fGPow, fGDec,
  fGInvPow, fGInvDec,

) where

import Data.Array.Accelerate                                        ( Acc, Array, DIM2, Exp, Elt, Z(..), (:.)(..) )
import qualified Data.Array.Accelerate                              as A

import qualified Data.Array.Accelerate.Algebra.ToInteger            as ToInteger ()
import qualified Data.Array.Accelerate.Algebra.Additive             as Additive ()
import qualified Data.Array.Accelerate.Algebra.IntegralDomain       as IntegralDomain ()
import qualified Data.Array.Accelerate.Algebra.Ring                 as Ring ()
import qualified Data.Array.Accelerate.Algebra.ZeroTestable         as ZeroTestable
import Data.Array.Accelerate.Algebra.ZeroTestable                   ( isZero )

import Crypto.Lol.Cyclotomic.Tensor.Accelerate.Common
import Crypto.Lol.Cyclotomic.Tensor.Accelerate.Backend
import Crypto.Lol.LatticePrelude                                    hiding ( ZeroTestable, isZero )


type ZeroTestable a = ZeroTestable.C a


-- | Arbitrary-index @L@ transform, which converts from decoding-basis to
-- powerful-basis representation
--
fL :: (Fact m, Additive (Exp r), Elt r) => Arr m r -> Arr m r
fL = eval $ fTensor $ ppTensor pL

-- | Arbitrary-index @L^{-1}@ transform, which converts from powerful-basis to
-- decoding-basis representation
--
fLInv :: (Fact m, Additive (Exp r), Elt r) => Arr m r -> Arr m r
fLInv = eval $ fTensor $ ppTensor pLInv

-- | Arbitrary-index multiplication by @g_m@ in the powerful basis
--
fGPow :: (Fact m, Additive (Exp r), Elt r) => Arr m r -> Arr m r
fGPow = eval $ fTensor $ ppTensor pGPow

-- | Arbitrary-index multiplication by @g_m@ in the decoding basis
--
fGDec :: (Fact m, Additive (Exp r), Elt r) => Arr m r -> Arr m r
fGDec = eval $ fTensor $ ppTensor pGDec

-- | Arbitrary-index division by @g_m@ in the powerful basis. Outputs 'Nothing'
-- if not evenly divisible by @g_m@.
--
-- Unfortunately, we have an extra 'IsNum' constraint because the
-- numeric-prelude definition of 'fromIntegral' requires an instance of
-- 'ToIntegrer', which is entirely untenable for Accelerate.
--
-- WARNING: Not constant time.
--
fGInvPow
    :: (Fact m, IntegralDomain (Exp r), ZeroTestable (Exp r), Elt r)
    => Arr m r
    -> Maybe (Arr m r)
fGInvPow = wrapGInv pGInvPow

-- | Arbitrary-index division by @g_m@ in the decoding basis. Outputs 'Nothing'
-- if not evenly divisible by @g_m@.
--
-- WARNING: Not constant time
--
fGInvDec
    :: (Fact m, IntegralDomain (Exp r), ZeroTestable (Exp r), Elt r)
    => Arr m r
    -> Maybe (Arr m r)
fGInvDec = wrapGInv pGInvDec


-- Implementations
-- ---------------

pWrap :: forall p r. Prim p
      => (Int -> Acc (Array DIM2 r) -> Acc (Array DIM2 r))
      -> Tagged p (Trans r)
pWrap f
  | pval > 2  = return $ trans (pval - 1, f pval)
  | otherwise = return $ Id 1
  where
    pval = proxy valuePrime (Proxy :: Proxy p)


pL :: (Prim p, Additive (Exp r), Elt r) => Tagged p (Trans r)
pL = pWrap $ \_ arr -> scanl1_2d (+) arr

pLInv :: forall p r. (Prim p, Additive (Exp r), Elt r) => Tagged p (Trans r)
pLInv = pWrap $ \_ arr ->
  let
      f :: Exp DIM2 -> Exp r
      f ix = let Z :. y :. x = A.unlift ix
                 u           = arr A.! ix
                 v           = x A.>* 0 A.? ( arr A.! (A.index2 y (x-1)), zero )
             in u - v
  in
  A.generate (A.shape arr) f

-- Multiplication by g_p = 1 - zeta_p in the power basis.
--
-- Note that this is incorrect for p==2, but we should never use that case
-- thanks to pWrap.
--
pGPow :: forall p r. (Prim p, Additive (Exp r), Elt r) => Tagged p (Trans r)
pGPow = pWrap $ \p arr ->
  let
      f :: Exp DIM2 -> Exp r
      f ix = let Z :. y :. x = A.unlift ix
                 u           = arr A.! ix
                 v           = arr A.! A.index2 y (A.constant (p-2))
                 w           = x A.>* 0 A.? ( arr A.! A.index2 y (x-1) , zero )
             in
             u + v - w
  in
  A.generate (A.shape arr) f

-- Multiplication by g_p == 1 - zeta_p in the decoding basis
--
pGDec :: forall p r. (Prim p, Additive (Exp r), Elt r) => Tagged p (Trans r)
pGDec = pWrap $ \_ arr ->
  let
      s = A.fold (+) zero arr

      f :: Exp DIM2 -> Exp r
      f ix = let Z :. y :. x = A.unlift ix
                 u           = arr A.! ix
                 v           = x A.==* 0 A.? ( s A.! A.index1 y
                                             , negate (arr A.! A.index2 x (x-1)) )
             in
             u + v
  in
  A.generate (A.shape arr) f


wrapGInv
    :: forall m r. (Fact m, IntegralDomain (Exp r), ZeroTestable (Exp r), Elt r)
    => (forall p. Prim p => Tagged p (Trans r))
    -> Arr m r
    -> Maybe (Arr m r)
wrapGInv gInv =
  let
      fGInv = eval $ fTensor $ ppTensor gInv
      rad   = fromIntegral (proxy oddRadicalFact (Proxy :: Proxy m))
  in
  flip divCheck rad . fGInv

divCheck
    :: (IntegralDomain (Exp r), ZeroTestable (Exp r), Elt r)
    => Arr m r
    -> Exp r
    -> Maybe (Arr m r)
divCheck arr den =
  let
      (q,r)    = A.unzip $ A.map (\x -> A.lift (x `divMod` den)) (unArr arr)
      ok       = A.all isZero r
      (ok',q') = run (A.lift (ok,q))  -- TODO: Don't copy 'q' back to the host
  in
  if A.indexArray ok' Z
     then Just (Arr (A.use q'))
     else Nothing

-- Doesn't do division by (odd) p
--
pGInvPow :: forall p r. (Prim p, Ring (Exp r), Elt r) => Tagged p (Trans r)
pGInvPow = pWrap $ \p arr ->
  let
      sl   = scanl1_2d (+) arr
      sr   = scanr1_2d (+) arr

      f :: Exp DIM2 -> Exp r -> Exp r -> Exp r
      f ix x y  = let i = A.indexHead ix
                  in  fromIntegral (A.constant p-i-1) * x
                    + fromIntegral (-i-1)           * y
  in
  A.izipWith f sl sr


-- Doesn't do division by (odd) p
--
pGInvDec :: forall p r. (Prim p, Ring (Exp r), Elt r) => Tagged p (Trans r)
pGInvDec = pWrap $ \p arr ->
  let
      nats = A.generate (A.shape arr) (\ix -> fromIntegral (A.indexHead ix) + one)
      sl   = A.fold (+) zero (A.zipWith (*) arr nats)
      sr   = scanr1_2d (+) arr

      f :: Exp DIM2 -> Exp r -> Exp r
      f ix x = let Z :. j :. _ = A.unlift ix :: Z :. Exp Int :. Exp Int
                   s           = sl A.! A.index1 j
               in
               s - fromIntegral (A.constant p) * x
  in
  A.imap f sr


-- Accelerate doesn't have native multi-dimensional scans, so implement them
-- here via segmented scans. Segmented scans are similarly not a primitive
-- operation, so we may be able to do this more efficiently still.
--
scanl1_2d :: Elt a => (Exp a -> Exp a -> Exp a) -> Acc (Array DIM2 a) -> Acc (Array DIM2 a)
scanl1_2d f arr = A.reshape (A.shape arr) p
  where
    Z :. h :. w = A.unlift (A.shape arr)
    seg         = A.fill (A.index1 h) w
    vec         = A.flatten arr
    p           = A.scanl1Seg f vec seg

scanr1_2d :: Elt a => (Exp a -> Exp a -> Exp a) -> Acc (Array DIM2 a) -> Acc (Array DIM2 a)
scanr1_2d f arr = A.reshape (A.shape arr) p
  where
    Z :. h :. w = A.unlift (A.shape arr)
    seg         = A.fill (A.index1 h) w
    vec         = A.flatten arr
    p           = A.scanr1Seg f vec seg

