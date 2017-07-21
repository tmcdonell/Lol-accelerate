{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
-- |
-- Module      : Crypto.Lol.Cyclotomic.Tensor.Accelerate.Extension
-- Copyright   : [2016] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Functions for embedding and twacing in various bases
--

module Crypto.Lol.Cyclotomic.Tensor.Accelerate.Extension (

  twacePowDec,
  twaceCRT,
  coeffs,
  crtSetDec,

) where

-- accelerate (& friends)
import Data.Array.Accelerate                                        ( Array, DIM1, DIM2, Exp, Elt, FromIntegral, Z(..), (:.)(..) )
import Data.Array.Accelerate.IO                                     as A
import qualified Data.Array.Accelerate                              as A

-- lol/lol-accelerate
import Crypto.Lol.Cyclotomic.Tensor.Accelerate.Backend
import Crypto.Lol.Cyclotomic.Tensor.Accelerate.Common
import qualified Crypto.Lol.Cyclotomic.Tensor.Accelerate.CRT        as CRT

import Data.Array.Accelerate.Crypto.Lol.CRTrans

import Crypto.Lol.Prelude                                           as P hiding (FromIntegral)
import Crypto.Lol.Types.FiniteField
import Crypto.Lol.Types.ZmStar
import qualified Crypto.Lol.Cyclotomic.Tensor                       as T

-- other libraries
import Data.Maybe
import Data.Reflection                                              ( reify )
import qualified Data.Vector                                        as V
import qualified Data.Vector.Storable                               as S


-- | The "tweaked trace" function in either the powerful or decoding basis of
-- the m'`th cyclotomic ring to the m`th cyclotomic ring when @m | m'@.
--
twacePowDec
    :: forall m m' r. (m `Divides` m', Elt r)
    => Arr m' r
    -> Arr m  r
twacePowDec (Arr arr) =
  let indices = proxy extIndicesPowDec (Proxy::Proxy '(m,m'))
      !go     = runN A.gather
  in  Arr $ go indices arr


-- | The "tweaked trace" function in the CRT basis of the m'`th cyclotomic ring
-- to the m`th cyclotomic ring, when @m | m'@
--
twaceCRT
    :: forall monad m m' r. (m `Divides` m', CRTIndex (Exp r) ~ Exp Int, CRTrans monad (Exp r), FromIntegral Int r, Elt r)
    => monad (Arr m' r -> Arr m r)
twaceCRT = do
  g             <- CRT.gCRT     :: monad (Arr m' r)
  gInv          <- CRT.gInvCRT  :: monad (Arr m r)
  embed         <- CRT.embed    :: monad (Arr m r -> Arr m' r)
  (_, m'hatInv) <- proxyT crtInfo (Proxy::Proxy m') :: monad (CRTInfo (Exp r))
  let
      vhf         = proxy valueHatFact  (Proxy::Proxy m)
      indices     = proxy extIndicesCRT (Proxy::Proxy '(m,m'))
      hatRatioInv = m'hatInv * A.fromIntegral (A.constant vhf)
      --
      tweak xs ys                     -- tweak = mhat * g' / (m'hat * g)
        = A.map (* hatRatioInv)
        $ A.zipWith (*) xs ys

      go ix xs ys zs                  -- take true trace after mul-by-tweak
        = A.fold (+) zero
        . A.gather ix
        . A.zipWith (*) zs
        $ tweak xs ys
  --
  return $ \(Arr arr) -> Arr $! runN go indices (unArr (embed gInv)) (unArr g) arr


-- | Map an array in the powerful/decoding basis, representing an @O_m'@
-- element, to an array of arrays representing @O_m@ elements in the same type
-- of basis
--
coeffs :: forall m m' r. (m `Divides` m', Elt r)
       =>  Arr m' r
       -> [Arr m r]
coeffs (Arr arr) =
  let indices = proxy extIndicesCoeffs (Proxy::Proxy '(m,m'))
      !go     = runN (flip A.gather)
  in
  V.toList $ V.map (Arr . go arr) indices


-- | A list of arrays representing the mod-@p@ CRT set of the extension
-- @O_m' / O_m@.
--
-- NOTE: This is computed sequentially on the host!
--
crtSetDec
    :: forall m m' fp. (m `Divides` m', PrimeField fp, Coprime (PToF (CharOf fp)) m', Elt fp)
    => Tagged m [Arr m' fp]
crtSetDec =
  let
      p        = proxy valuePrime   (Proxy::Proxy (CharOf fp))
      phi      = proxy totientFact  (Proxy::Proxy m')
      d        = proxy (order p)    (Proxy::Proxy m')
      h        = proxy valueHatFact (Proxy::Proxy m')
      cosets   = proxy (partitionCosets p) (Proxy::Proxy '(m,m'))
      zmsToIdx = proxy T.zmsToIndexFact (Proxy::Proxy m')
      hinv     = recip (fromIntegral h) :: fp
  in
  return $ reify d $ \(Proxy :: Proxy d) ->
    let
        -- Internally, finite fields are represented as a polynomial whose
        -- coefficients are stored in a list, which is not representable in Exp.
        twCRTs :: T.Kron (GF fp d)
        twCRTs = fromMaybe (error "Internal error: Accelerate.crtSetDec")
               $ proxyT T.twCRTs (Proxy::Proxy m')

        trace' :: GF fp d -> fp   -- To avoid recomputing powTraces
        trace' = trace

        index j i   = T.indexK twCRTs j (zmsToIdx i)
        f is (Z:.j) = hinv * trace' (P.sum [ index j i | i <- is ])
    in
    P.map (Arr . A.fromFunction (Z:.phi) . f) cosets


-- Reindexing arrays
-- -----------------
--
-- TODO: Make sure these really are memoised.
--

-- NOTE:
--  * conversion from storable to Accelerate vector is O(1)
--
extIndicesPowDec
    :: (m `Divides` m')
    => Tagged '(m,m') (Array DIM1 Int)
extIndicesPowDec = do
  idxs   <- T.extIndicesPowDec
  return $! A.fromVectors (Z :. S.length idxs) idxs

extIndicesCRT
    :: forall m m'. (m `Divides` m')
    => Tagged '(m,m') (Array DIM2 Int)
extIndicesCRT = do
  let
      phi  = proxy totientFact (Proxy::Proxy m)
      phi' = proxy totientFact (Proxy::Proxy m')
      sh   = Z :. phi :. phi' `div` phi
  --
  idxs   <- T.extIndicesCRT
  return $! A.fromVectors sh idxs

extIndicesCoeffs
    :: (m `Divides` m')
    => Tagged '(m,m') (V.Vector (Array DIM1 Int))
extIndicesCoeffs = do
  idxss  <- T.extIndicesCoeffs
  return $! V.map (\idxs -> A.fromVectors (Z :. S.length idxs) idxs) idxss

