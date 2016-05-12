{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE PolyKinds             #-}
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

  CRTrans(..),
  CRTEmbed(..)

) where

import Data.Array.Accelerate                                        as A hiding ( (*), (/) )
import qualified Data.Array.Accelerate.Algebra.Additive             as Additive ()
import qualified Data.Array.Accelerate.Algebra.Ring                 as Ring ()
import qualified Data.Array.Accelerate.Algebra.Transcendental       as Transcendental ()

import Data.Array.Accelerate.Crypto.Lol.Types.Complex               as A
import Data.Array.Accelerate.Crypto.Lol.Reflects

import Crypto.Lol.Cyclotomic.Tensor.Accelerate.AT

import Crypto.Lol.CRTrans
import Crypto.Lol.LatticePrelude                                    as LP


-- CRTrans
-- -------

-- CRTrans for product rings
--
instance (CRTrans monad (Exp i) (Exp a), CRTrans monad (Exp i) (Exp b), Elt a, Elt b)
    => CRTrans monad (Exp i) (Exp (a,b)) where
  crtInfo = do
    (fa, inva :: Exp a) <- crtInfo
    (fb, invb :: Exp b) <- crtInfo
    return (\i -> A.lift (fa i, fb i), A.lift (inva, invb))

-- Complex numbers have roots of unity of any order
--
instance (Monad monad, Transcendental (Exp a), A.Num a, A.FromIntegral Int a, Elt a)
    => CRTrans monad (Exp Int) (Exp (Complex a)) where
  crtInfo = crtInfoC

crtInfoC
    :: forall monad m a. (Monad monad, Reflects m (Exp Int), Transcendental (Exp a), A.Num a, A.FromIntegral Int a, Elt a)
    => TaggedT m monad (CRTInfo (Exp Int) (Exp (Complex a)))
crtInfoC =
  let
      mval = proxy value (Proxy::Proxy m)
      mhat = let (d,m) = LP.divMod mval 2 -- this is @valueHat mval@ lifted to Exp
             in  m ==* 0 ? ( d, mval )
  in
  return ( omegaPowC mhat
         , LP.recip (A.fromReal (A.fromIntegral mhat))
         )

omegaPowC
    :: (Transcendental (Exp a), A.Num a, A.FromIntegral Int a, Elt a)
    => Exp Int
    -> Exp Int
    -> Exp (Complex a)
omegaPowC m i = A.cis (2 * LP.pi * A.fromIntegral i / A.fromIntegral m)

-- CRTrans instances for real and integer types, which do not have roots of
-- unity (except in trivial cases).
--
instance CRTrans Maybe (Exp Int) (Exp Double) where crtInfo = tagT Nothing
instance CRTrans Maybe (Exp Int) (Exp Int)    where crtInfo = tagT Nothing
instance CRTrans Maybe (Exp Int) (Exp Int64)  where crtInfo = tagT Nothing



-- CRTEmbed
-- --------

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

