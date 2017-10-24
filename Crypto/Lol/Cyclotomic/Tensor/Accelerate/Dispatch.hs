{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
-- |
-- Module      : Crypto.Lol.Cyclotomic.Tensor.Accelerate.Dispatch
-- Copyright   : [2016] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Crypto.Lol.Cyclotomic.Tensor.Accelerate.Dispatch
  where

import Control.Applicative
import Crypto.Lol.Prelude                                           as LP hiding ( FromIntegral )

import Crypto.Lol.Cyclotomic.Tensor.Accelerate.Backend
import Crypto.Lol.Cyclotomic.Tensor.Accelerate.Common               as Arr
import qualified Crypto.Lol.Cyclotomic.Tensor.Accelerate.CRT        as CRT
import qualified Crypto.Lol.Cyclotomic.Tensor.Accelerate.Dec        as Dec
import qualified Crypto.Lol.Cyclotomic.Tensor.Accelerate.Extension  as Ext
import qualified Crypto.Lol.Cyclotomic.Tensor.Accelerate.GL         as GL
import qualified Crypto.Lol.Cyclotomic.Tensor.Accelerate.Pow        as Pow


import Data.Array.Accelerate                                        as A
import Data.Array.Accelerate.Crypto.Lol.CRTrans
import qualified Data.Array.Accelerate.Algebra.ZeroTestable         as ZeroTestable


-- These implement the operations of Tensor, and should be compiled
-- ahead-of-time for each ring/moduli combination they are used at.
--
-- This should work for the functions of a single moduli, but operations between
-- moduli such as 'Ext.twaceCRT' could be problematic...
--

scalarPow' :: forall m r. (Fact m, Additive (Exp r), Elt r) => r -> Arr m r
scalarPow' = Arr . go . scalar
  where
    !go = runN (proxy Pow.scalar (Proxy::Proxy m))

fL' :: forall m r. (Fact m, Additive (Exp r), Elt r) => Arr m r -> Arr m r
fL' = Arr.wrap go
  where
    !go = runN (proxy GL.fL (Proxy::Proxy m))

fLInv' :: forall m r. (Fact m, Additive (Exp r), Elt r) => Arr m r -> Arr m r
fLInv' = Arr.wrap go
  where
    !go = runN (proxy GL.fLInv (Proxy::Proxy m))

mulGPow' :: forall m r. (Fact m, Additive (Exp r), Elt r) => Arr m r -> Arr m r
mulGPow' = Arr.wrap go
  where
    !go = runN (proxy GL.fGPow (Proxy::Proxy m))

mulGDec' :: forall m r. (Fact m, Additive (Exp r), Elt r) => Arr m r -> Arr m r
mulGDec' = Arr.wrap go
  where
    !go = runN (proxy GL.fGDec (Proxy::Proxy m))

divGPow'
    :: forall m r. (Fact m, IntegralDomain (Exp r), ZeroTestable.C (Exp r), FromIntegral Int r, Elt r)
    => Arr m r
    -> Maybe (Arr m r)
divGPow' = Arr.wrapM check
  where
    !go     = runN (proxy GL.fGInvPow (Proxy::Proxy m))
    check x = let (ok,r) = go x in
              if indexArray ok Z
                then Just r
                else Nothing

divGDec'
    :: forall m r. (Fact m, IntegralDomain (Exp r), ZeroTestable.C (Exp r), FromIntegral Int r, Elt r)
    => Arr m r
    -> Maybe (Arr m r)
divGDec' = Arr.wrapM check
  where
    !go     = runN (proxy GL.fGInvDec (Proxy::Proxy m))
    check x = let (ok,r) = go x in
                if indexArray ok Z
                  then Just r
                  else Nothing

scalarCRT' :: forall monad m r. (CRTrans monad (Exp r), Fact m, Elt r) => monad (r -> Arr m r)
scalarCRT' = return $ Arr . go . scalar
  where
    !go = runN (proxy CRT.scalar (Proxy::Proxy m))

mulGCRT' :: forall monad m r. (CRTrans monad (Exp r), CRTIndex (Exp r) ~ Exp Int, Fact m, Elt r)
         => monad (Arr m r -> Arr m r)
mulGCRT' = Arr.wrap <$> go
  where
    !go = do f <- CRT.mulGCRT
             return $ runN (proxy f (Proxy::Proxy m))

divGCRT' :: forall monad m r. (CRTrans monad (Exp r), CRTIndex (Exp r) ~ Exp Int, FromIntegral Int r, Fact m, Elt r)
         => monad (Arr m r -> Arr m r)
divGCRT' = Arr.wrap <$> go
  where
    !go = do f <- CRT.divGCRT
             return $ runN (proxy f (Proxy::Proxy m))

fCRT' :: forall monad m r. (CRTrans monad (Exp r), CRTIndex (Exp r) ~ Exp Int, Fact m, Elt r)
      => monad (Arr m r -> Arr m r)
fCRT' = Arr.wrap <$> go
  where
    !go = do f <- CRT.fCRT
             return $ runN (proxy f (Proxy::Proxy m))

fCRTInv' :: forall monad m r. (CRTrans monad (Exp r), CRTIndex (Exp r) ~ Exp Int, Fact m, Elt r)
         => monad (Arr m r -> Arr m r)
fCRTInv' = Arr.wrap <$> go
  where
    !go = do f <- CRT.fCRTInv
             return $ runN (proxy f (Proxy::Proxy m))

gSqNormDec' :: forall m r. (Fact m, Ring (Exp r), Elt r) => Arr m r -> r
gSqNormDec' (Arr arr) = go arr `indexArray` Z
  where
    !go = runN (proxy Dec.gSqNorm (Proxy::Proxy m))

twacePowDec' :: forall m m' r. (m `Divides` m', Elt r) => Arr m' r -> Arr m r
twacePowDec' = Arr.wrap (go indices)
  where
    !go       = runN gather
    !indices  = proxy Ext.extIndicesPowDec (Proxy::Proxy '(m,m'))

embedPow' :: forall m m' r. (m `Divides` m', Additive (Exp r), Elt r) => Arr m r -> Arr m' r
embedPow' = Arr.wrap (go indices)
  where
    !go       = runN Pow.embed'
    !indices  = proxy Pow.baseIndicesPow (Proxy::Proxy '(m,m'))

embedDec' :: forall m m' r. (m `Divides` m', Additive (Exp r), Elt r) => Arr m r -> Arr m' r
embedDec' = Arr.wrap (go indices)
  where
    !go       = runN Dec.embed'
    !indices  = proxy Dec.baseIndicesDec (Proxy::Proxy '(m,m'))

twaceCRT'
    :: forall monad m m' r. (m `Divides` m', CRTrans monad (Exp r), CRTIndex (Exp r) ~ Exp Int, FromIntegral Int r, Elt r)
    => monad (Arr m' r -> Arr m r)
twaceCRT' = Arr.wrap <$> go
  where
    !go = do f <- Ext.twaceCRT
             return $ runN (proxy f (Proxy::Proxy '(m,m')))

embedCRT' :: forall monad m m' r. (m `Divides` m', CRTrans monad (Exp r), Elt r) => monad (Arr m r -> Arr m' r)
embedCRT' = do
  _ <- proxyT crtInfo (Proxy::Proxy m') :: monad (CRTInfo (Exp r))
  return $ Arr.wrap (go indices)
  where
    !go     = runN CRT.embed'
    indices = proxy CRT.baseIndicesCRT (Proxy::Proxy '(m,m'))


-- Utility
-- -------

scalar :: Elt e => e -> Scalar e
scalar x  = fromList Z [x]



