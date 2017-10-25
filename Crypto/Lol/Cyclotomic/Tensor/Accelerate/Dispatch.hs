{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
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
-- This module implements the operations of Tensor
--
-- Ideally, we would pre-compile these at each type they are used at. However,
-- as the current `runQ` infrastructure is built around TemplateHaskell, and the
-- implementation of the Tensor operations make heavy use of meta-programming
-- around the modulus type(s) `m`, this is quite difficult.
--
-- As GHC is not magical enough to reuse our invocations of `runN` itself, we
-- must explicitly memoise each call at a given modulus/ring combination. There
-- is of course some overhead to this, but much less than going through the
-- entire Accelerate pipeline.
--

module Crypto.Lol.Cyclotomic.Tensor.Accelerate.Dispatch
  where

import Control.Applicative
import System.IO.Unsafe
import Crypto.Lol.Prelude                                           as LP hiding ( FromIntegral )

import Crypto.Lol.Cyclotomic.Tensor.Accelerate.Backend
import Crypto.Lol.Cyclotomic.Tensor.Accelerate.Memo
import Crypto.Lol.Cyclotomic.Tensor.Accelerate.Common               as Arr
import qualified Crypto.Lol.Cyclotomic.Tensor.Accelerate.CRT        as CRT
import qualified Crypto.Lol.Cyclotomic.Tensor.Accelerate.Dec        as Dec
import qualified Crypto.Lol.Cyclotomic.Tensor.Accelerate.Extension  as Ext
import qualified Crypto.Lol.Cyclotomic.Tensor.Accelerate.GL         as GL
import qualified Crypto.Lol.Cyclotomic.Tensor.Accelerate.Pow        as Pow

import Data.Array.Accelerate                                        as A
import Data.Array.Accelerate.Crypto.Lol.CRTrans
import qualified Data.Array.Accelerate.Algebra.ZeroTestable         as ZeroTestable


scalarPow' :: forall m r. (Fact m, Additive (Exp r), Elt r) => r -> Arr m r
scalarPow' = Arr . go . scalar
  where
    !go = memo __scalarPow' (MK::MemoKey '(m,r))
        $ runN (proxy Pow.scalar (Proxy::Proxy m))

fL' :: forall m r. (Fact m, Additive (Exp r), Elt r) => Arr m r -> Arr m r
fL' = Arr.wrap go
  where
    !go = memo __fL' (MK::MemoKey '(m,r))
        $ runN (proxy GL.fL (Proxy::Proxy m))

fLInv' :: forall m r. (Fact m, Additive (Exp r), Elt r) => Arr m r -> Arr m r
fLInv' = Arr.wrap go
  where
    !go = memo __fLInv' (MK::MemoKey '(m,r))
        $ runN (proxy GL.fLInv (Proxy::Proxy m))

mulGPow' :: forall m r. (Fact m, Additive (Exp r), Elt r) => Arr m r -> Arr m r
mulGPow' = Arr.wrap go
  where
    !go = memo __mulGPow' (MK::MemoKey '(m,r))
        $ runN (proxy GL.fGPow (Proxy::Proxy m))

mulGDec' :: forall m r. (Fact m, Additive (Exp r), Elt r) => Arr m r -> Arr m r
mulGDec' = Arr.wrap go
  where
    !go = memo __mulGDec' (MK::MemoKey '(m,r))
        $ runN (proxy GL.fGDec (Proxy::Proxy m))

divGPow'
    :: forall m r. (Fact m, IntegralDomain (Exp r), ZeroTestable.C (Exp r), FromIntegral Int r, Elt r)
    => Arr m r
    -> Maybe (Arr m r)
divGPow' = Arr.wrapM check
  where
    !go      = memo __divGPow' (MK::MemoKey '(m,r))
             $ runN (proxy GL.fGInvPow (Proxy::Proxy m))
    check x  = let (ok,r) = go x in
               if indexArray ok Z
                 then Just r
                 else Nothing

divGDec'
    :: forall m r. (Fact m, IntegralDomain (Exp r), ZeroTestable.C (Exp r), FromIntegral Int r, Elt r)
    => Arr m r
    -> Maybe (Arr m r)
divGDec' = Arr.wrapM check
  where
    !go      = memo __divGDec' (MK::MemoKey '(m,r))
             $ runN (proxy GL.fGInvDec (Proxy::Proxy m))
    check x  = let (ok,r) = go x in
                 if indexArray ok Z
                   then Just r
                   else Nothing

scalarCRT' :: forall monad m r. (CRTrans monad (Exp r), Fact m, Elt r) => monad (r -> Arr m r)
scalarCRT' = return $ Arr . go . scalar
  where
    !go = memo __scalarCRT' (MK::MemoKey '(m,r))
        $ runN (proxy CRT.scalar (Proxy::Proxy m))

mulGCRT' :: forall monad m r. (CRTrans monad (Exp r), CRTIndex (Exp r) ~ Exp Int, Fact m, Elt r)
         => monad (Arr m r -> Arr m r)
mulGCRT' = Arr.wrap <$> go
  where
    !go = memo __mulGCRT' (MK::MemoKey '(m,r))
        $ do f <- CRT.mulGCRT
             return $ runN (proxy f (Proxy::Proxy m))

divGCRT' :: forall monad m r. (CRTrans monad (Exp r), CRTIndex (Exp r) ~ Exp Int, FromIntegral Int r, Fact m, Elt r)
         => monad (Arr m r -> Arr m r)
divGCRT' = Arr.wrap <$> go
  where
    !go = memo __divGCRT' (MK::MemoKey '(m,r))
        $ do f <- CRT.divGCRT
             return $ runN (proxy f (Proxy::Proxy m))

fCRT' :: forall monad m r. (CRTrans monad (Exp r), CRTIndex (Exp r) ~ Exp Int, Fact m, Elt r)
      => monad (Arr m r -> Arr m r)
fCRT' = Arr.wrap <$> go
  where
    !go = memo __fCRT' (MK::MemoKey '(m,r))
        $ do f <- CRT.fCRT
             return $ runN (proxy f (Proxy::Proxy m))

fCRTInv' :: forall monad m r. (CRTrans monad (Exp r), CRTIndex (Exp r) ~ Exp Int, Fact m, Elt r)
         => monad (Arr m r -> Arr m r)
fCRTInv' = Arr.wrap <$> go
  where
    !go = memo __fCRTInv' (MK::MemoKey '(m,r))
        $ do f <- CRT.fCRTInv
             return $ runN (proxy f (Proxy::Proxy m))

gSqNormDec' :: forall m r. (Fact m, Ring (Exp r), Elt r) => Arr m r -> r
gSqNormDec' (Arr arr) = go arr `indexArray` Z
  where
    !go = memo __gSqNormDec' (MK::MemoKey '(m,r))
        $ runN (proxy Dec.gSqNorm (Proxy::Proxy m))

twacePowDec' :: forall m m' r. (m `Divides` m', Elt r) => Arr m' r -> Arr m r
twacePowDec' = Arr.wrap go
  where
    indices = proxy Ext.extIndicesPowDec (Proxy::Proxy '(m,m'))
    !go     = memo __twacePowDec' (MK::MemoKey '(m,m',r))
            $ runN (gather (use indices))

embedPow' :: forall m m' r. (m `Divides` m', Additive (Exp r), Elt r) => Arr m r -> Arr m' r
embedPow' = Arr.wrap go
  where
    indices = proxy Pow.baseIndicesPow (Proxy::Proxy '(m,m'))
    !go     = memo __embedPow' (MK::MemoKey '(m,m',r))
            $ runN (Pow.embed' (use indices))

embedDec' :: forall m m' r. (m `Divides` m', Additive (Exp r), Elt r) => Arr m r -> Arr m' r
embedDec' = Arr.wrap go
  where
    indices = proxy Dec.baseIndicesDec (Proxy::Proxy '(m,m'))
    !go     = memo __embedDec' (MK::MemoKey '(m,m',r))
            $ runN (Dec.embed' (use indices))

twaceCRT'
    :: forall monad m m' r. (m `Divides` m', CRTrans monad (Exp r), CRTIndex (Exp r) ~ Exp Int, FromIntegral Int r, Elt r)
    => monad (Arr m' r -> Arr m r)
twaceCRT' = Arr.wrap <$> go
  where
    !go = memo __twaceCRT' (MK::MemoKey '(m,m',r))
        $ do f <- Ext.twaceCRT
             return $ runN (proxy f (Proxy::Proxy '(m,m')))

embedCRT' :: forall monad m m' r. (m `Divides` m', CRTrans monad (Exp r), Elt r) => monad (Arr m r -> Arr m' r)
embedCRT' = do
  _ <- proxyT crtInfo (Proxy::Proxy m') :: monad (CRTInfo (Exp r))
  return $ Arr.wrap go
  where
    indices = proxy CRT.baseIndicesCRT (Proxy::Proxy '(m,m'))
    !go     = memo __embedCRT' (MK::MemoKey '(m,m',r))
            $ runN (CRT.embed' (use indices))


-- Utility
-- -------

scalar :: Elt e => e -> Scalar e
scalar x  = fromList Z [x]


-- Memoisation
-- -----------

-- Explicit memo tables for each of the tensor functions

{-# NOINLINE __scalarPow' #-}
__scalarPow' :: MemoTable '(m,r) (Scalar r -> Vector r)
__scalarPow' = unsafePerformIO newMemoTable

{-# NOINLINE __fL'    #-}
{-# NOINLINE __fLInv' #-}
__fL', __fLInv' :: MemoTable '(m,r) (Vector r -> Vector r)
__fL'    = unsafePerformIO newMemoTable
__fLInv' = unsafePerformIO newMemoTable

{-# NOINLINE __mulGPow' #-}
{-# NOINLINE __mulGDec' #-}
__mulGPow', __mulGDec' :: MemoTable '(m,r) (Vector r -> Vector r)
__mulGPow' = unsafePerformIO newMemoTable
__mulGDec' = unsafePerformIO newMemoTable

{-# NOINLINE __divGPow' #-}
{-# NOINLINE __divGDec' #-}
__divGPow', __divGDec' :: MemoTable '(m,r) (Vector r -> (Scalar Bool, Vector r))
__divGPow' = unsafePerformIO newMemoTable
__divGDec' = unsafePerformIO newMemoTable

{-# NOINLINE __scalarCRT' #-}
__scalarCRT' :: MemoTable '(m,r) (Scalar r -> Vector r)
__scalarCRT' = unsafePerformIO newMemoTable

{-# NOINLINE __mulGCRT' #-}
{-# NOINLINE __divGCRT' #-}
__mulGCRT', __divGCRT' :: MemoTable '(m,r) (monad (Vector r -> Vector r))
__mulGCRT' = unsafePerformIO newMemoTable
__divGCRT' = unsafePerformIO newMemoTable

{-# NOINLINE __fCRT'    #-}
{-# NOINLINE __fCRTInv' #-}
__fCRT', __fCRTInv' :: MemoTable '(m,r) (monad (Vector r -> Vector r))
__fCRT'    = unsafePerformIO newMemoTable
__fCRTInv' = unsafePerformIO newMemoTable

{-# NOINLINE __gSqNormDec' #-}
__gSqNormDec' :: MemoTable '(m,r) (Vector r -> Scalar r)
__gSqNormDec' = unsafePerformIO newMemoTable

{-# NOINLINE __twacePowDec' #-}
__twacePowDec' :: MemoTable '(m,m',r) (Vector r -> Vector r)
__twacePowDec' = unsafePerformIO newMemoTable

{-# NOINLINE __embedPow' #-}
{-# NOINLINE __embedDec' #-}
__embedPow', __embedDec' :: MemoTable '(m,m',r) (Vector r -> Vector r)
__embedPow'    = unsafePerformIO newMemoTable
__embedDec'    = unsafePerformIO newMemoTable

{-# NOINLINE __twaceCRT' #-}
__twaceCRT' :: MemoTable '(m,m',r) (monad (Vector r -> Vector r))
__twaceCRT' = unsafePerformIO newMemoTable

{-# NOINLINE __embedCRT' #-}
__embedCRT' :: MemoTable '(m,m',r) (Vector r -> Vector r)
__embedCRT' = unsafePerformIO newMemoTable

