{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NoImplicitPrelude   #-}
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

) where

-- accelerate
import Data.Array.Accelerate                                        as A hiding ( (+), (*), div )
import Data.Array.Accelerate.IO                                     as A
-- import Data.Array.Accelerate.Array.Data

-- lol/lol-accelerate
import Crypto.Lol.Cyclotomic.Tensor.Accelerate.Common
import qualified Crypto.Lol.Cyclotomic.Tensor.Accelerate.CRT        as CRT

import Data.Array.Accelerate.Crypto.Lol.CRTrans

import Crypto.Lol.LatticePrelude                                    as P
import qualified Crypto.Lol.Cyclotomic.Tensor                       as T

-- other libraries
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
  in  Arr $ A.map (arr A.!!) indices


-- | The "tweaked trace" function in the CRT basis of the m'`th cyclotomic ring
-- to the m`th cyclotomic ring, when @m | m'@
--
twaceCRT
    :: forall monad m m' r. (m `Divides` m', CRTrans monad (Exp Int) (Exp r), FromIntegral Int r, Elt r)
    => monad (Arr m' r -> Arr m r)
twaceCRT = do
  g             <- CRT.gCRT     :: monad (Arr m' r)
  gInv          <- CRT.gInvCRT  :: monad (Arr m r)
  embed         <- CRT.embed    :: monad (Arr m r -> Arr m' r)
  (_, m'hatInv) <- proxyT crtInfo (Proxy::Proxy m') :: monad (CRTInfo (Exp Int) (Exp r))
  let
      vhf         = proxy valueHatFact  (Proxy::Proxy m)
      indices     = proxy extIndicesCRT (Proxy::Proxy '(m,m'))
      hatRatioInv = m'hatInv * A.fromIntegral (A.constant vhf)
      tweak xs ys = A.map (* hatRatioInv)
                  $ A.zipWith (*) (unArr xs) (unArr ys)
  --
  return $ \(Arr arr) ->
    let
        xs = A.zipWith (*) arr $ tweak (embed gInv) g
        ys = A.map (xs A.!!) indices
        zs = A.fold (+) zero ys
    in
    Arr zs


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
    => Tagged '(m,m') (Acc (Vector Int))
extIndicesPowDec = do
  idxs           <- T.extIndicesPowDec
  return . A.use $! A.fromVectors (Z :. S.length idxs) idxs

extIndicesCRT
    :: forall m m'. (m `Divides` m')
    => Tagged '(m,m') (Acc (Array DIM2 Int))
extIndicesCRT = do
  let
      phi  = proxy totientFact (Proxy::Proxy m)
      phi' = proxy totientFact (Proxy::Proxy m')
      sh   = Z :. phi :. phi' `div` phi
  --
  idxs           <- T.extIndicesCRT
  return . A.use $! A.fromVectors sh idxs

