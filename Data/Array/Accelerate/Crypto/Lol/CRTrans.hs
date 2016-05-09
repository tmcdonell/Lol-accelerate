{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.Crypto.Lol.CRTrans
-- Copyright   : [2016] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Crypto.Lol.CRTrans (

  CRTEmbed(..)

) where

import Data.Array.Accelerate                                        as A
import qualified Data.Array.Accelerate.Algebra.Additive             as Additive ()
import qualified Data.Array.Accelerate.Algebra.Ring                 as Ring ()
import qualified Data.Array.Accelerate.Algebra.Transcendental       as Transcendental ()

import Data.Array.Accelerate.Crypto.Lol.Types.Complex               as A

import Crypto.Lol.Cyclotomic.Tensor.Accelerate.AT

import Crypto.Lol.CRTrans
import Crypto.Lol.LatticePrelude                                    as LP


-- Instance for product rings
instance (CRTEmbed AT a, CRTEmbed AT b, Elt a, Elt b, Elt (CRTExt a), Elt (CRTExt b))
    => CRTEmbed AT (a,b) where
  type CRTExt (a,b) = (CRTExt a, CRTExt b)
  --
  toExt   = tag $ \t -> let x = proxy toExt (Proxy::Proxy (AT m a)) (A.fst t)
                            y = proxy toExt (Proxy::Proxy (AT m b)) (A.snd t)
                        in
                        A.lift (x,y)
  --
  fromExt = tag $ \t -> let x = proxy fromExt (Proxy::Proxy (AT m a)) (A.fst t)
                            y = proxy fromExt (Proxy::Proxy (AT m b)) (A.snd t)
                        in
                        A.lift (x,y)

-- Trivial instance for complex numbers
instance (Transcendental (Exp a), Elt a) => CRTEmbed AT (Complex a) where
  type CRTExt (Complex a) = Complex a
  toExt   = tag id
  fromExt = tag id

-- Instances for real and integral types embed into Complex
instance CRTEmbed AT Int where
  type CRTExt Int = Complex Double
  toExt   = tag $ A.fromReal . A.fromIntegral
  fromExt = tag $ A.round . A.real

instance CRTEmbed AT Int64 where
  type CRTExt Int64 = Complex Double
  toExt   = tag $ A.fromReal . A.fromIntegral
  fromExt = tag $ A.round . A.real

instance CRTEmbed AT Double where
  type CRTExt Double = Complex Double
  toExt   = tag A.fromReal
  fromExt = tag A.real

-- Arbitrary-precision computations are not supported in Accelerate at this time
-- instance CRTEmbed AT Integer

