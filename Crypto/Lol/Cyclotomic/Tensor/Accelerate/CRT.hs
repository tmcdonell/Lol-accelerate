{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
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

module Crypto.Lol.Cyclotomic.Tensor.Accelerate.CRT (

  scalarCRT,
  fCRT, fCRTInv,
  gCRT, gInvCRT,
  mulGCRT, divGCRT,

) where

import Data.Array.Accelerate                                        as A

import qualified Data.Array.Accelerate.Algebra.ToInteger            as ToInteger ()
import qualified Data.Array.Accelerate.Algebra.Additive             as Additive ()
import qualified Data.Array.Accelerate.Algebra.IntegralDomain       as IntegralDomain ()
import qualified Data.Array.Accelerate.Algebra.Ring                 as Ring ()

import Crypto.Lol.Cyclotomic.Tensor.Accelerate.Matrix
import Crypto.Lol.Cyclotomic.Tensor.Accelerate.Common

import Crypto.Lol.CRTrans
import Crypto.Lol.LatticePrelude                                    as P

import Control.Applicative
import Data.Singletons.Prelude                                      ( Sing(..), sing)


-- | Embeds a scalar into the CRT basis (when it exists)
--
scalarCRT
    :: forall monad m r. (Fact m, Elt r, Monad monad)
    => monad (Exp r -> Arr m r)
scalarCRT =
  let n  = totientPPs (proxy ppsFact (Proxy :: Proxy m))
      sh = constant (Z :. n)
  in
  return $ Arr . A.fill sh


-- | Multiply by @g_m@ in the CRT basis (when it exists)
--
mulGCRT
    :: forall monad m r. (Fact m, CRTrans monad (Exp Int) (Exp r), Ring (Exp r), Elt r)
    => monad (Arr m r -> Arr m r)
mulGCRT =
  let go :: Arr m r -> Arr m r -> Arr m r
      go g x = Arr $ A.zipWith (*) (unArr x) (unArr g)
  in
  go <$> gCRT

-- | Divide by @g_m@ in the CRT basis (when it exists)
--
divGCRT
    :: forall monad m r. (Fact m, CRTrans monad (Exp Int) (Exp r), Ring (Exp r), Elt r)
    => monad (Arr m r -> Arr m r)
divGCRT =
  let go :: Arr m r -> Arr m r -> Arr m r
      go g x = Arr $ A.zipWith (*) (unArr x) (unArr g)
  in
  go <$> gInvCRT


-- | The coefficient vector of @g@ in the CRT basis (when it exists)
--
gCRT :: (Fact m, CRTrans monad (Exp Int) (Exp r), Ring (Exp r), Elt r)
     => monad (Arr m r)
gCRT = wrapVector gCRTM

gCRTM
    :: (Fact m, CRTrans monad (Exp Int) (Exp r), Ring (Exp r), Elt r)
    => TaggedT m monad (Matrix r)
gCRTM = fMatrix gCRTPPow

gCRTPPow
    :: (PPow pp, CRTrans monad (Exp Int) (Exp r), Ring (Exp r), Elt r)
    => TaggedT pp monad (MatrixC r)
gCRTPPow = ppMatrix gCRTPrime

-- | A @(p-1)@-by-1 matrix of the CRT coefficients of @g_p@, for the @p^e@th
-- cyclotomic ring.
--
gCRTPrime
    :: forall monad p r. (Prim p, CRTrans monad (Exp Int) (Exp r), Ring (Exp r), Elt r)
    => TaggedT p monad (MatrixC r)
gCRTPrime = do
  p         <- pureT valuePrime
  (wPow, _) <- crtInfo
  return $ if p <= 2
              then MC (Z :. 1   :. 1) (\_  -> one)
              else MC (Z :. p-1 :. 1) (\ix -> let Z:.i:._ = unlift ix :: Z :. Exp Int :. Exp Int
                                              in  one - wPow (i+1))


-- | The coefficient vector of @g^{-1}@ in the CRT basis (when it exists)
--
gInvCRT
    :: (Fact m, CRTrans monad (Exp Int) (Exp r), Ring (Exp r), Elt r)
    => monad (Arr m r)
gInvCRT = wrapVector gInvCRTM

gInvCRTM
    :: (Fact m, CRTrans monad (Exp Int) (Exp r), Elt r)
    => TaggedT m monad (Matrix r)
gInvCRTM = fMatrix gInvCRTPPow

gInvCRTPPow
    :: (PPow pp, CRTrans monad (Exp Int) (Exp r), Elt r)
    => TaggedT pp monad (MatrixC r)
gInvCRTPPow = ppMatrix gInvCRTPrime

-- | The @(p-1)@-by-1 matrix of inverse-CRT coefficients of @g_p@ for a @p@th
-- cyclotomic ring.
--
gInvCRTPrime
    :: forall monad p r. (Prim p, CRTrans monad (Exp Int) (Exp r), Elt r)
    => TaggedT p monad (MatrixC r)
gInvCRTPrime = do
  p               <- pureT valuePrime
  (wPow, phatInv) <- crtInfo
  let
      -- XXX: Below is the implementation copied straight from RepaTensor,
      --      however this expression is potentially very large (depending on
      --      'p'), so may be better to implement as a simple 2D reduction
      --      (which follows).
      --
      --      Be wary of introducing nested parallelism...
      --
      -- f :: Exp DIM2 -> Exp r
      -- f _ | p <= 2 = one
      -- f ix         =
      --   let Z :. i :. _ = unlift ix :: Z :. Exp Int :. Exp Int
      --   in  phatInv * P.sum [ P.fromIntegral j * wPow ((i+1) * constant (p-1-j))
      --                       | j <- [1..p-1] ]

      arr :: Acc (Vector r)
      arr = A.fold (+) zero
          $ A.generate (constant (Z :. p-1 :. p-2))
                       (\ix -> let Z :. i :. j = unlift ix
                               in  P.fromIntegral (j+1) * wPow ((i+1) * (constant p - j)))

      f :: Exp DIM2 -> Exp r
      f ix = phatInv * arr ! indexTail ix
  --
  return $ MC (Z :. p-1 :. 1) f


-- | The Chinese Remainder Theorem exists iff CRT exists for all prime powers
--
fCRT :: (Fact m, CRTrans monad (Exp Int) (Exp r), Elt r) => monad (Arr m r -> Arr m r)
fCRT = evalM (fTensor ppCRT)

-- | The inverse Chinese Remainder theorem. Exists iff CRT exists for all prime
-- powers. Divides by m̂ after doing crtInv.
--
fCRTInv
    :: forall monad m r. (Fact m, CRTrans monad (Exp Int) (Exp r), Ring (Exp r), Elt r)
    => monad (Arr m r -> Arr m r)
fCRTInv = do
  (_, mhatInv) <- proxyT crtInfo (Proxy :: Proxy m) :: monad (CRTInfo (Exp Int) (Exp r))
  let
      totm    = proxy totientFact (Proxy :: Proxy m)
      divMhat = trans (totm, A.map (* mhatInv))
  --
  evalM $ (divMhat .*) <$> fTensor ppCRTInv


-- Operations over Prime Powers
-- ----------------------------

ppDFT
    :: forall monad pp r. (PPow pp, CRTrans monad (Exp Int) (Exp r), Elt r)
    => TaggedT pp monad (Trans r)
ppDFT = go (sing :: SPrimePower pp)
  where
    go :: Sing pp -> TaggedT pp monad (Trans r)
    go     (SPP (STuple2 sp SO))      = tagT $ withWitnessT pDFT sp
    go spp@(SPP (STuple2 sp (SS se))) = tagT $ do
      pp'dft <- withWitnessT ppDFT (SPP (STuple2 sp se))
      pptwid <- withWitnessT (ppTwid False) spp
      pdft   <- withWitnessT pDFT sp
      return
        $ (pp'dft @* Id (dim pdft))
       .* pptwid
       .* (Id (dim pp'dft) @* pdft)


ppCRT
    :: forall monad pp r. (PPow pp, CRTrans monad (Exp Int) (Exp r), Elt r)
    => TaggedT pp monad (Trans r)
ppCRT = go (sing :: SPrimePower pp)
  where
    go :: Sing pp -> TaggedT pp monad (Trans r)
    go     (SPP (STuple2 sp SO))       = tagT $ withWitnessT pCRT sp
    go spp@(SPP (STuple2 sp (SS se'))) = tagT $ do
      pp'dft <- withWitnessT ppDFT (SPP (STuple2 sp se'))
      pptwid <- withWitnessT (ppTwidHat False) spp
      pcrt   <- withWitnessT pCRT sp
      return
        $ (pp'dft @* Id (dim pcrt))
       .* pptwid
       .* (if dim pcrt > 1 then Id (dim pp'dft) @* pcrt
                           else Id (dim pp'dft))         -- save some work when p==2


ppDFTInv
    :: forall pp monad r. (PPow pp, CRTrans monad (Exp Int) (Exp r), Elt r)
    => TaggedT pp monad (Trans r)
ppDFTInv = go (sing :: SPrimePower pp)
  where
    go :: Sing pp -> TaggedT pp monad (Trans r)
    go     (SPP (STuple2 sp SO))      = tagT $ withWitnessT pDFTInv sp
    go spp@(SPP (STuple2 sp (SS se))) = tagT $ do
      pp'dftInv <- withWitnessT ppDFTInv (SPP (STuple2 sp se))
      pptwidInv <- withWitnessT (ppTwid True) spp
      pdftInv   <- withWitnessT pDFTInv sp
      return
        $ (Id (dim pp'dftInv) @* pdftInv)
       .* pptwidInv
       .* (pp'dftInv @* Id (dim pdftInv))


ppCRTInv
    :: forall monad pp r. (PPow pp, CRTrans monad (Exp Int) (Exp r), Elt r)
    => TaggedT pp monad (Trans r)
ppCRTInv = go (sing :: SPrimePower pp)
  where
    go :: Sing pp -> TaggedT pp monad (Trans r)
    go     (SPP (STuple2 sp SO))      = tagT $ withWitnessT pCRTInv sp
    go spp@(SPP (STuple2 sp (SS se))) = tagT $ do
      pp'dftInv <- withWitnessT ppDFTInv (SPP (STuple2 sp se))
      pptwidInv <- withWitnessT (ppTwidHat True) spp
      pcrtInv   <- withWitnessT pCRTInv sp
      return
        $ (Id (dim pp'dftInv) @* pcrtInv)
       .* pptwidInv
       .* (pp'dftInv @* Id (dim pcrtInv))


-- Twiddle factors for DFT_pp and CRT_pp representations
--
ppTwid
    :: forall monad pp r. (PPow pp, CRTrans monad (Exp Int) (Exp r), Elt r)
    => Bool
    -> TaggedT pp monad (Trans r)
ppTwid inv = do
  (omegaPPPow, _) <- crtInfo
  let
      pp@(p,e) = proxy ppPPow (Proxy :: Proxy pp)
      ppval    = valuePP pp

      diag :: Acc (Vector r)
      diag = A.generate (constant (Z :. ppval))
           $ \ix -> let (iq,ir)         = indexHead ix `divMod` constant p
                        pow'            = ir * digitRev p (e-1) iq
                        pow | inv       = negate pow'
                            | otherwise = pow'
                    in
                    omegaPPPow pow
  --
  return $ trans (ppval, mulDiag diag)

ppTwidHat
    :: forall monad pp r. (PPow pp, CRTrans monad (Exp Int) (Exp r), Elt r)
    => Bool
    -> TaggedT pp monad (Trans r)
ppTwidHat inv = do
  (omegaPPPow, _) <- crtInfo
  let
      pp@(p,e) = proxy ppPPow (Proxy :: Proxy pp)
      pptot    = totientPP pp

      diag :: Acc (Vector r)
      diag = A.generate (constant (Z :. pptot))
           $ \ix -> let (iq,ir)         = indexHead ix `divMod` constant (p-1)
                        pow'            = (ir+1) * digitRev p (e-1) iq
                        pow | inv       = negate pow'
                            | otherwise = pow'
                    in
                    omegaPPPow pow
  --
  return $ trans (pptot, mulDiag diag)


-- | Base-p digit reversal. Input and output are in @p^e@.
--
digitRev :: Int -> Int -> Exp Int -> Exp Int
digitRev p e j
  | e < 1     = zero
  | otherwise = let (q,r) = j `divMod` constant p
                in  r * (constant (p ^ (e-1))) + digitRev p (e-1) q


-- Operations over Prim
-- --------------------

pDFT :: forall monad p r. (Prim p, CRTrans monad (Exp Int) (Exp r), Elt r)
     => TaggedT p monad (Trans r)
pDFT = do
  (omegaPPow, _) <- crtInfo
  let
      pval = proxy valuePrime (Proxy :: Proxy p)
      mat  = A.generate (constant (Z :. pval :. pval))
           $ \ix -> let Z :. i :. j = unlift ix :: Z :. Exp Int :. Exp Int
                    in  omegaPPow (i*j)
  --
  return $ if pval == 2
             then butterfly
             else trans (pval, mulMat mat)

pCRT :: forall monad p r. (Prim p, CRTrans monad (Exp Int) (Exp r), Elt r)
     => TaggedT p monad (Trans r)
pCRT = do
  (omegaPPow, _) <- crtInfo
  let
      pval = proxy valuePrime (Proxy :: Proxy p)
      mat  = A.generate (constant (Z :. pval-1 :. pval-1))
           $ \ix -> let Z :. i :. j = unlift ix :: Z :. Exp Int :. Exp Int
                    in  omegaPPow ((i+1)*j)
  --
  return $ if pval == 2
              then Id 1
              else trans (pval-1, mulMat mat)

pDFTInv
    :: forall monad p r. (Prim p, CRTrans monad (Exp Int) (Exp r), Elt r)
    => TaggedT p monad (Trans r)
pDFTInv = do
  (omegaPPow, _) <- crtInfo
  let
      pval = proxy valuePrime (Proxy :: Proxy p)
      mat  = A.generate (constant (Z :. pval :. pval))
           $ \ix -> let Z :. i :. j = unlift ix :: Z :. Exp Int :. Exp Int
                    in  omegaPPow (-i*j)
  --
  return $ if pval == 2
              then butterfly
              else trans (pval, mulMat mat)

-- pCRT * pCRTInv = \hat{p}*I, for all prime p
pCRTInv
    :: forall monad p r. (Prim p, CRTrans monad (Exp Int) (Exp r), Elt r)
    => TaggedT p monad (Trans r)
pCRTInv = do
  (omegaPPow, _) <- crtInfo
  let
      pval = proxy valuePrime (Proxy :: Proxy p)
      mat  = A.generate (constant (Z :. pval-1 :. pval-1))
           $ \ix -> let Z :. i :. j = unlift ix :: Z :. Exp Int :. Exp Int
                    in  omegaPPow (negate i * (j+1)) -
                        omegaPPow (j+1)
  --
  return $ if pval == 2
              then Id 1
              else trans (pval-1, mulMat mat)


butterfly :: forall r. (Additive (Exp r), Elt r) => Trans r
butterfly = trans (2, flap)
  where
    flap :: Acc (Array DIM2 r) -> Acc (Array DIM2 r)
    flap arr =
      let
          sh :: Exp DIM2
          sh = A.lift (indexTail (shape arr) :. constant 2)

          f :: Exp DIM2 -> Exp r
          f ix = let Z :. i :. j = unlift ix
                     x           = arr ! index2 i 0
                     y           = arr ! index2 i 1
                 in
                 j ==* 0 ? ( x+y, x-y )
      in
      A.generate sh f


-- Utilities
-- ---------

wrapVector
    :: forall monad m r. (Monad monad, Fact m, Ring (Exp r), Elt r)
    => TaggedT m monad (Matrix r)
    -> monad (Arr m r)
wrapVector v = do
  vmat <- proxyT v (Proxy :: Proxy m)
  let n  = proxy totientFact (Proxy :: Proxy m)
      sh = constant (Z :. n)
      --
      f :: Exp DIM1 -> Exp r
      f ix = indexM vmat (A.lift (ix :. constant 0))
  --
  return . Arr $ A.generate sh f

