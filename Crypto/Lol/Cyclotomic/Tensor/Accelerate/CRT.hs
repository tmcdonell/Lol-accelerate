{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Crypto.Lol.Cyclotomic.Tensor.Accelerate.CRT
-- Copyright   : [2016] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Functions to support Chinese Remainder Theorem (CRT) operations in
-- Accelerate.
--

module Crypto.Lol.Cyclotomic.Tensor.Accelerate.CRT
  where

import Data.Array.Accelerate                                        as A

import qualified Data.Array.Accelerate.Algebra.Additive             as Additive ()
import qualified Data.Array.Accelerate.Algebra.Ring                 as Ring ()

import Crypto.Lol.Cyclotomic.Tensor.Accelerate.GL
import Crypto.Lol.Cyclotomic.Tensor.Accelerate.Common

import Crypto.Lol.CRTrans
import Crypto.Lol.LatticePrelude

import Control.Applicative
import Data.Singletons.Prelude


-- | Embeds a scalar into the CRT basis (when it exists)
--
scalarCRT
    :: forall monad m r. (Fact m, CRTrans monad r, Elt r)
    => monad (Exp r -> Arr m r)
scalarCRT =
  let
      n  = totientPPs (proxy ppsFact (Proxy :: Proxy m))
      sh = constant (Z :. n)
  in
  return $ Arr . A.fill sh


-- | Multiply by @g_m@ in the CRT basis (when it exists)
--
mulGCRT :: forall monad m r. (Fact m, CRTrans monad r, Ring (Exp r), Elt r) => monad (Arr m r -> Arr m r)
mulGCRT =
  let go :: Arr m r -> Arr m r -> Arr m r
      go g x = Arr $ A.zipWith (*) (unArr x) (unArr g)
  in
  go <$> gCRT

-- | Divide by @g_m@ in the CRT basis (when it exists)
--
divGCRT :: forall monad m r. (Fact m, CRTrans monad r, Ring (Exp r), Elt r) => monad (Arr m r -> Arr m r)
divGCRT =
  let go :: Arr m r -> Arr m r -> Arr m r
      go g x = Arr $ A.zipWith (*) (unArr x) (unArr g)
  in
  go <$> gInvCRT


-- | The coefficient vector of @g@ in the CRT basis (when it exists)
--
gCRT :: (Fact m, CRTrans monad r, Ring (Exp r), Elt r) => monad (Arr m r)
gCRT = fCRT <*> return (fGPow (scalarPow one))

-- | The coefficient vector of @g^{-1}@ in the CRT basis (when it exists)
--
gInvCRT :: (Fact m, CRTrans monad r, Ring (Exp r), Elt r) => monad (Arr m r)
gInvCRT = undefined

-- | The Chinese Remainder Theorem exists iff CRT exists for all prime powers
--
fCRT :: (Fact m, CRTrans monad r, Elt r) => monad (Arr m r -> Arr m r)
fCRT = evalM (fTensor ppCRT)


ppDFT :: (PPow pp, CRTrans monad r, Elt r) => TaggedT pp monad (Trans r)
ppDFT = undefined


ppCRT :: forall monad pp r. (PPow pp, CRTrans monad r, Elt r) => TaggedT pp monad (Trans r)
ppCRT = go (sing :: SPrimePower pp)
  where
    go :: Sing pp -> TaggedT pp monad (Trans r)
    go     (SPP (STuple2 sp SO))       = tagT $ withWitnessT pCRT sp
    go spp@(SPP (STuple2 sp (SS se'))) = tagT $ do
      pp'dft <- withWitnessT ppDFT (SPP (STuple2 sp se'))
      pptwid <- withWitnessT (ppTwidHat False) spp
      pcrt   <- withWitnessT pCRT sp
      return $
        (pp'dft @* Id (dim pcrt)) .* pptwid .*
        -- save some work when p==2
        (if dim pcrt > 1 then Id (dim pp'dft) @* pcrt
                         else Id (dim pp'dft))


pCRT :: forall monad p r. (Prim p, CRTrans monad r, Elt r) => TaggedT p monad (Trans r)
pCRT = undefined

-- Twiddle factors for DFT_pp and CRT_pp representations
--
ppTwid :: (PPow pp, CRTrans monad r, Elt r) => Bool -> TaggedT pp monad (Trans r)
ppTwid = undefined

ppTwidHat :: (PPow pp, CRTrans monad r, Elt r) => Bool -> TaggedT pp monad (Trans r)
ppTwidHat = undefined

