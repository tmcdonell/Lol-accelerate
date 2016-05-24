{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
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
  CRTEmbed(..),
  CRTInfo

) where

import Data.Array.Accelerate                                        as A hiding ( (*), (/) )
import qualified Data.Array.Accelerate.Algebra.Additive             as Additive ()
import qualified Data.Array.Accelerate.Algebra.Ring                 as Ring ()
import qualified Data.Array.Accelerate.Algebra.Transcendental       as Transcendental ()

import Data.Array.Accelerate.Crypto.Lol.Types.Complex               as A

import Crypto.Lol.CRTrans
import Crypto.Lol.LatticePrelude                                    as LP
import Crypto.Lol.Reflects


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
    :: forall monad m a. (Monad monad, Reflects m Int, Transcendental (Exp a), A.Num a, A.FromIntegral Int a, Elt a)
    => TaggedT m monad (CRTInfo (Exp Int) (Exp (Complex a)))
crtInfoC =
  let
      mval = constant $ proxy value (Proxy::Proxy m)
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
instance ( CRTEmbed (Exp a), Elt a, Elt (CRTExt a), A.Lift Exp (CRTExt (Exp a)), CRTExt (Exp a) ~ Exp (CRTExt a)
         , CRTEmbed (Exp b), Elt b, Elt (CRTExt b), A.Lift Exp (CRTExt (Exp b)), CRTExt (Exp b) ~ Exp (CRTExt b)
         )
    => CRTEmbed (Exp (a,b)) where
  type CRTExt (Exp (a,b)) = Exp (CRTExt a, CRTExt b)
  --
  toExt t = let x = toExt (A.fst t) :: CRTExt (Exp a)
                y = toExt (A.snd t) :: CRTExt (Exp b)
            in
            A.lift (x,y)
  --
  fromExt t = let x = fromExt (A.fst t) :: Exp a
                  y = fromExt (A.snd t) :: Exp b
              in
              A.lift (x,y)

-- Trivial instance for complex numbers
instance (Transcendental (Exp a), Elt a) => CRTEmbed (Exp (Complex a)) where
  type CRTExt (Exp (Complex a)) = Exp (Complex a)
  toExt   = id
  fromExt = id

-- Instances for real and integral types embed into Complex
instance CRTEmbed (Exp Int) where
  type CRTExt (Exp Int) = Exp (Complex Double)
  toExt   = A.fromReal . A.fromIntegral
  fromExt = A.round . A.real

instance CRTEmbed (Exp Int64) where
  type CRTExt (Exp Int64) = Exp (Complex Double)
  toExt   = A.fromReal . A.fromIntegral
  fromExt = A.round . A.real

instance CRTEmbed (Exp Double) where
  type CRTExt (Exp Double) = Exp (Complex Double)
  toExt   = A.fromReal
  fromExt = A.real

-- Arbitrary-precision computations are not supported in Accelerate at this time
-- instance CRTEmbed AT Integer

