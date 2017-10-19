{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
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

  scalar,
  embed,
  fCRT, fCRTInv,
  gCRT, gInvCRT,
  mulGCRT, divGCRT,

  baseIndicesCRT,

  -- prim
  embedCRT',

) where

-- accelerate (& friends)
import Data.Array.Accelerate                                        ( Acc, Array, Scalar, Vector, DIM1, DIM2, Exp, Elt, Z(..), (:.)(..) )
import Data.Array.Accelerate.IO                                     as A
import qualified Data.Array.Accelerate                              as A

import qualified Data.Array.Accelerate.Algebra.ToInteger            as ToInteger ()
import qualified Data.Array.Accelerate.Algebra.Additive             as Additive ()
import qualified Data.Array.Accelerate.Algebra.IntegralDomain       as IntegralDomain ()
import qualified Data.Array.Accelerate.Algebra.Ring                 as Ring ()

-- lol/lol-accelerate
import Crypto.Lol.Cyclotomic.Tensor.Accelerate.Backend
import Crypto.Lol.Cyclotomic.Tensor.Accelerate.Common               hiding ( scalar )
import Crypto.Lol.Cyclotomic.Tensor.Accelerate.Matrix
import Crypto.Lol.Cyclotomic.Tensor.Accelerate.Prim                 ( Dispatch )
import qualified Crypto.Lol.Cyclotomic.Tensor.Accelerate.Prim       as P

import Crypto.Lol.CRTrans
import Crypto.Lol.Prelude                                           as P hiding (Matrix)
import qualified Crypto.Lol.Cyclotomic.Tensor                       as T

-- other libraries
import Control.Applicative
import Data.Singletons.Prelude                                      ( Sing(..), sing)
import qualified Data.Vector.Storable                               as S


-- | Embeds a scalar into the CRT basis (when it exists)
--
-- NOTE: This does not get pre-compiled.
--
scalar
    :: forall m r. (Fact m, Elt r)
    => r
    -> Arr m r
scalar r =
  let n  = proxy totientFact (Proxy :: Proxy m)
      sh = A.constant (Z :. n)
  in
  Arr . run $ A.fill sh (A.constant r)


-- | Embeds an array in the CRT basis of the m`th cyclotomic ring into an array
-- in the CRT basis of the m'`th cyclotomic ring, when @m | m'@.
--
embed :: forall monad m m' r. (m `Divides` m', CRTrans monad (Exp r), Dispatch r)
      => monad (Arr m r -> Arr m' r)
embed = do
  -- first check existence of the CRT transform in m'
  _ <- proxyT crtInfo (Proxy::Proxy m') :: monad (CRTInfo (Exp r))
  let indices = proxy baseIndicesCRT (Proxy::Proxy '(m,m'))
  return $ wrap (P.embedCRT' indices)

embedCRT' :: Elt r => Acc (Vector Int) -> Acc (Vector r) -> Acc (Vector r)
embedCRT' = A.gather


-- | Multiply by @g_m@ in the CRT basis (when it exists)
--
mulGCRT
    :: forall monad m r. (Fact m, CRTrans monad (Exp r), Elt r, CRTIndex (Exp r) ~ Exp Int)
    => monad (Tagged m (Acc (Vector r) -> Acc (Vector r)))
mulGCRT = do
  g <- gCRT :: monad (Tagged m (Acc (Vector r)))
  return . tag $ A.zipWith (*) (untag g)

-- | Divide by @g_m@ in the CRT basis (when it exists)
--
divGCRT
    :: forall monad m r. (Fact m, CRTrans monad (Exp r), CRTIndex (Exp r) ~ Exp Int, A.FromIntegral Int r, Elt r)
    => monad (Tagged m (Acc (Vector r) -> Acc (Vector r)))
divGCRT = do
  gInv <- gInvCRT :: monad (Tagged m (Acc (Vector r)))
  return . tag $ A.zipWith (*) (untag gInv)


-- | The coefficient vector of @g@ in the CRT basis (when it exists)
--
gCRT :: (Fact m, CRTrans monad (Exp r), Elt r, CRTIndex (Exp r) ~ Exp Int)
     => monad (Tagged m (Acc (Vector r)))
gCRT = wrapVector gCRTM

gCRTM
    :: (Fact m, CRTrans monad (Exp r), CRTIndex (Exp r) ~ Exp Int)
    => TaggedT m monad (Matrix r)
gCRTM = fMatrix gCRTPPow

gCRTPPow
    :: (PPow pp, CRTrans monad (Exp r), CRTIndex (Exp r) ~ Exp Int)
    => TaggedT pp monad (MatrixC r)
gCRTPPow = ppMatrix gCRTPrime

-- | A @(p-1)@-by-1 matrix of the CRT coefficients of @g_p@, for the @p^e@th
-- cyclotomic ring.
--
gCRTPrime
    :: forall monad p r. (Prime p, CRTrans monad (Exp r), CRTIndex (Exp r) ~ Exp Int)
    => TaggedT p monad (MatrixC r)
gCRTPrime = do
  p         <- pureT valuePrime
  (wPow, _) <- crtInfo
  return $ if p <= 2
              then MC (Z :. 1   :. 1) (\_  -> one)
              else MC (Z :. p-1 :. 1) (\ix -> let Z:.i:._ = A.unlift ix :: Z :. Exp Int :. Exp Int
                                              in  one - wPow (i+1))


-- | The coefficient vector of @g^{-1}@ in the CRT basis (when it exists)
--
gInvCRT
    :: (Fact m, CRTrans monad (Exp r), A.FromIntegral Int r, Elt r, CRTIndex (Exp r) ~ Exp Int)
    => monad (Tagged m (Acc (Vector r)))
gInvCRT = wrapVector gInvCRTM

gInvCRTM
    :: (Fact m, CRTrans monad (Exp r), A.FromIntegral Int r, Elt r, CRTIndex (Exp r) ~ Exp Int)
    => TaggedT m monad (Matrix r)
gInvCRTM = fMatrix gInvCRTPPow

gInvCRTPPow
    :: (PPow pp, CRTrans monad (Exp r), A.FromIntegral Int r, Elt r, CRTIndex (Exp r) ~ Exp Int)
    => TaggedT pp monad (MatrixC r)
gInvCRTPPow = ppMatrix gInvCRTPrime

-- | The @(p-1)@-by-1 matrix of inverse-CRT coefficients of @g_p@ for a @p@th
-- cyclotomic ring.
--
gInvCRTPrime
    :: forall monad p r . (Prime p, CRTrans monad (Exp r), A.FromIntegral Int r, Elt r, CRTIndex (Exp r) ~ Exp Int)
    => TaggedT p monad (MatrixC r)
gInvCRTPrime = do
  p               <- pureT valuePrime
  (wPow, phatInv) <- crtInfo
  let
      -- TLM: At the moment this cutoff is dictated more by Accelerate's
      --      optimisation stage rather than performance considerations ):
      f | p <= 2    = const one
        | p < 16    = f_seq     -- TODO: tune me
        | otherwise = f_par

      -- Sequential implementation copied straight from RepaTensor. However,
      -- this expression is potentially very large (depending on 'p'), so for
      -- large sizes we compute this as a parallel reduction (below).
      --
      f_seq :: Exp DIM2 -> Exp r
      f_seq ix =
        let Z :. i :. _ = A.unlift ix :: Z :. Exp Int :. Exp Int
        in  phatInv * P.sum [ P.fromIntegral j * wPow ((i+1) * A.constant (p-1-j))
                            | j <- [1..p-1] ]

      -- Parallel version of the above
      --
      f_par :: Exp DIM2 -> Exp r
      f_par ix = phatInv * arr A.! A.indexTail ix

      arr :: Acc (Vector r)
      arr = A.fold (+) zero
          $ A.generate (A.constant (Z :. p-1 :. p-1))
                       (\ix -> let Z :. i :. j = A.unlift ix :: Z :. Exp Int :. Exp Int
                               in  A.fromIntegral (j+1) * wPow ((i+1) * (A.constant p-2-j)))
  --
  return $ MC (Z :. p-1 :. 1) f


-- | The Chinese Remainder Theorem exists iff CRT exists for all prime powers
--
fCRT :: (Fact m, CRTrans monad (Exp r), Elt r, CRTIndex (Exp r) ~ Exp Int)
     => monad (Tagged m (Acc (Vector r) -> Acc (Vector r)))
fCRT = evalM (fTensor ppCRT)

-- | The inverse Chinese Remainder theorem. Exists iff CRT exists for all prime
-- powers. Divides by m̂ after doing crtInv.
--
fCRTInv
    :: forall monad m r. (Fact m, CRTrans monad (Exp r), Elt r, CRTIndex (Exp r) ~ Exp Int)
    => monad (Tagged m (Acc (Vector r) -> Acc (Vector r)))
fCRTInv = do
  (_, mhatInv) <- proxyT crtInfo (Proxy :: Proxy m) :: monad (CRTInfo (Exp r))
  let
      totm    = proxy totientFact (Proxy :: Proxy m)
      divMhat = trans (totm, A.map (* mhatInv))
  --
  evalM $ (divMhat .*) <$> fTensor ppCRTInv


{-
-- | The "tweaked" CRT^* matrix:
--
--   @ CRT^* * diag ( sigma (g_p) ) @
--
twCRTs :: (Fact m, CRTrans monad (Exp r), Elt r)
       => TaggedT m monad (Matrix r)
twCRTs = fMatrix twCRTsPPow

-- | The "tweaked" CRT^* matrix for prime powers
--
twCRTsPPow
    :: (PPow pp, CRTrans monad (Exp r), Elt r)
    => TaggedT pp monad (MatrixC r)
twCRTsPPow = do
  phi         <- pureT totientPPow
  iToZms      <- pureT indexToZmsPPow
  jToPow      <- pureT indexToPowPPow
  (wPow,_)    <- crtInfo
  (MC _ gCRT) <- gCRTPPow
  let
      sh   = Z :. phi :. phi
      f ix = let Z :. j :. i = A.unlift ix :: Z :. Exp Int :. Exp Int
             in  wPow (jToPow j * negate (iToZms i)) * gCRT (A.index2 i 0)
  --
  return $ MC sh f
-}


-- Operations over Prime Powers
-- ----------------------------

ppDFT
    :: forall monad pp r. (PPow pp, CRTrans monad (Exp r), Elt r, CRTIndex (Exp r) ~ Exp Int)
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
    :: forall monad pp r. (PPow pp, CRTrans monad (Exp r), Elt r, CRTIndex (Exp r) ~ Exp Int)
    => TaggedT pp monad (Trans r)
ppCRT = go (sing :: SPrimePower pp)
  where
    go :: Sing pp -> TaggedT pp monad (Trans r)
    go     (SPP (STuple2 sp SO))      = tagT $ withWitnessT pCRT sp
    go spp@(SPP (STuple2 sp (SS se))) = tagT $ do
      pp'dft <- withWitnessT ppDFT (SPP (STuple2 sp se))
      pptwid <- withWitnessT (ppTwidHat False) spp
      pcrt   <- withWitnessT pCRT sp
      return
        $ (pp'dft @* Id (dim pcrt))
       .* pptwid
       .* (if dim pcrt > 1 then Id (dim pp'dft) @* pcrt
                           else Id (dim pp'dft))         -- save some work when p==2


ppDFTInv
    :: forall pp monad r. (PPow pp, CRTrans monad (Exp r), Elt r, CRTIndex (Exp r) ~ Exp Int)
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
    :: forall monad pp r. (PPow pp, CRTrans monad (Exp r), Elt r, CRTIndex (Exp r) ~ Exp Int)
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
    :: forall monad pp r. (PPow pp, CRTrans monad (Exp r), Elt r, CRTIndex (Exp r) ~ Exp Int)
    => Bool
    -> TaggedT pp monad (Trans r)
ppTwid inv = do
  (omegaPPPow, _) <- crtInfo
  let
      pp@(p,e) = proxy ppPPow (Proxy :: Proxy pp)
      ppval    = valuePP pp

      diag :: Acc (Vector r)
      diag = A.generate (A.constant (Z :. ppval))
           $ \ix -> let (iq,ir)         = A.indexHead ix `divMod` A.constant p
                        pow'            = ir * digitRev p (e-1) iq
                        pow | inv       = negate pow'
                            | otherwise = pow'
                    in
                    omegaPPPow pow
  --
  return $ trans (ppval, mulDiag diag)

ppTwidHat
    :: forall monad pp r. (PPow pp, CRTrans monad (Exp r), Elt r, CRTIndex (Exp r) ~ Exp Int)
    => Bool
    -> TaggedT pp monad (Trans r)
ppTwidHat inv = do
  (omegaPPPow, _) <- crtInfo
  let
      pp@(p,e) = proxy ppPPow (Proxy :: Proxy pp)
      pptot    = totientPP pp

      diag :: Acc (Vector r)
      diag = A.generate (A.constant (Z :. pptot))
           $ \ix -> let (iq,ir)         = A.indexHead ix `divMod` A.constant (p-1)
                        pow'            = (ir+1) * digitRev p (e-1) iq
                        pow | inv       = negate pow'
                            | otherwise = pow'
                    in
                    omegaPPPow pow
  --
  return $ trans (pptot, mulDiag diag)


-- Operations over Prime
-- --------------------

pDFT :: forall monad p r. (Prime p, CRTrans monad (Exp r), Elt r, CRTIndex (Exp r) ~ Exp Int)
     => TaggedT p monad (Trans r)
pDFT = do
  (omegaPPow, _) <- crtInfo
  let
      pval = proxy valuePrime (Proxy :: Proxy p)
      mat  = A.generate (A.constant (Z :. pval :. pval))
           $ \ix -> let Z :. i :. j = A.unlift ix :: Z :. Exp Int :. Exp Int
                    in  omegaPPow (i*j)
  --
  return $ if pval == 2
             then butterfly
             else trans (pval, mulMat mat)

pCRT :: forall monad p r. (Prime p, CRTrans monad (Exp r), Elt r, CRTIndex (Exp r) ~ Exp Int)
     => TaggedT p monad (Trans r)
pCRT = do
  (omegaPPow, _) <- crtInfo
  let
      pval = proxy valuePrime (Proxy :: Proxy p)
      mat  = A.generate (A.constant (Z :. pval-1 :. pval-1))
           $ \ix -> let Z :. i :. j = A.unlift ix :: Z :. Exp Int :. Exp Int
                    in  omegaPPow ((i+1)*j)
  --
  return $ if pval == 2
              then Id 1
              else trans (pval-1, mulMat mat)

pDFTInv
    :: forall monad p r. (Prime p, CRTrans monad (Exp r), Elt r, CRTIndex (Exp r) ~ Exp Int)
    => TaggedT p monad (Trans r)
pDFTInv = do
  (omegaPPow, _) <- crtInfo
  let
      pval = proxy valuePrime (Proxy :: Proxy p)
      mat  = A.generate (A.constant (Z :. pval :. pval))
           $ \ix -> let Z :. i :. j = A.unlift ix :: Z :. Exp Int :. Exp Int
                    in  omegaPPow (-i*j)
  --
  return $ if pval == 2
              then butterfly
              else trans (pval, mulMat mat)

-- pCRT * pCRTInv = \hat{p}*I, for all prime p
pCRTInv
    :: forall monad p r. (Prime p, CRTrans monad (Exp r), Elt r, CRTIndex (Exp r) ~ Exp Int)
    => TaggedT p monad (Trans r)
pCRTInv = do
  (omegaPPow, _) <- crtInfo
  let
      pval = proxy valuePrime (Proxy :: Proxy p)
      mat  = A.generate (A.constant (Z :. pval-1 :. pval-1))
           $ \ix -> let Z :. i :. j = A.unlift ix :: Z :. Exp Int :. Exp Int
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
          sh = A.lift (A.indexTail (A.shape arr) :. A.constant 2)

          f :: Exp DIM2 -> Exp r
          f ix = let Z :. i :. j = A.unlift ix
                     x           = arr A.! A.index2 i 0
                     y           = arr A.! A.index2 i 1
                 in
                 j A.== 0 A.? ( x+y, x-y )
      in
      A.generate sh f


-- Utilities
-- ---------

wrapVector
    :: forall monad m r. (Monad monad, Fact m, Ring (Exp r), Elt r)
    => TaggedT m monad (Matrix r)
    -> monad (Tagged m (Acc (Vector r)))
wrapVector v = do
  vmat <- proxyT v (Proxy :: Proxy m)
  let n  = proxy totientFact (Proxy :: Proxy m)
      sh = A.constant (Z :. n)
      --
      f :: Exp DIM1 -> Exp r
      f ix = indexM vmat (A.lift (ix :. A.constant 0))
  --
  return . tag $ A.generate sh f


-- Reindexing functions
-- --------------------

{-
indexToZmsPPow :: PPow pp => Tagged pp (Exp Int -> Exp Int)
indexToZmsPPow = indexToZms <$> ppPPow

indexToPowPPow :: PPow pp => Tagged pp (Exp Int -> Exp Int)
indexToPowPPow = indexToPow <$> ppPPow

-- | For a prime power @p^e@, map a tensor index to the corresponding element
-- @i@ in @Z_{p^e}^*@.
--
indexToZms :: PP -> Exp Int -> Exp Int
indexToZms (p,_) i =
  let (q,r) = i `divMod` (A.constant (p-1))
  in  A.constant p * q + r + 1

-- | For a prime power @p^e@, may a tensor index to the corresponding power @j@
-- in @[ phi(p^e) ]@, as in the powerful basis
--
indexToPow :: PP -> Exp Int -> Exp Int
indexToPow (p,e) j =
  let (q,r) = j `divMod` (A.constant (p-1))
  in  A.constant (p ^ (e-1)) * r + digitRev p (e-1) q

-- | Convert a @Z_m^*@ index to a linear tensor index in @[m]@
--
zmsToIndexFact :: Fact m => Tagged m (Exp Int -> Exp Int)
zmsToIndexFact = zmsToIndex <$> ppsFact

-- | Convert a @Z_m^*@ index to a linear tensor index
--
zmsToIndex :: [PP] -> Exp Int -> Exp Int
zmsToIndex []        _ = 0
zmsToInedx (pp:rest) i
  = zmsToIndexPP pp (i `mod` A.constant (valuePP pp))
  + A.constant (totientPP pp) * zmsToIndex rest i

-- | Inverse of 'indexToZms'
--
zmsToIndexPP :: PP -> Exp Int -> Exp Int
zmsToIndexPP (p,_) i =
  let (q,r) = i `divMod` A.constant p
  in  A.constant (p-1)*q + r - 1
-}

-- | Base-p digit reversal. Input and output are in @p^e@.
--
digitRev :: Int -> Int -> Exp Int -> Exp Int
digitRev p e j
  | e < 1     = zero
  | otherwise = let (q,r) = j `divMod` A.constant p
                in  r * (A.constant (p ^ (e-1))) + digitRev p (e-1) q


-- Reindexing arrays
-- -----------------
--
-- TODO: Make sure these really are memoised.
--

-- NOTE:
--  * conversion from storable to Accelerate vector is O(1)
--
baseIndicesCRT
    :: (m `Divides` m')
    => Tagged '(m,m') (Vector Int)
baseIndicesCRT = do
  idxs   <- T.baseIndicesCRT
  return $! A.fromVectors (Z :. S.length idxs) idxs

