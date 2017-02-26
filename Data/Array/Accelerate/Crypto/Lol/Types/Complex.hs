{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.Crypto.Lol.Types.Complex
-- Copyright   : [2016] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Crypto.Lol.Types.Complex
  where

import Data.Array.Accelerate                                        ( Lift(..), Unlift(..), Eq(..), (&&), (||), lift1, lift2 )
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Product
import Data.Array.Accelerate.Array.Sugar
import qualified Data.Array.Accelerate                              as A

import qualified Data.Array.Accelerate.Number.Complex               as Complex

import qualified Data.Array.Accelerate.Algebra.Absolute             as Absolute
import qualified Data.Array.Accelerate.Algebra.Additive             as Additive
import qualified Data.Array.Accelerate.Algebra.Algebraic            as Algebraic
import qualified Data.Array.Accelerate.Algebra.Field                as Field
import qualified Data.Array.Accelerate.Algebra.RealTranscendental   as RealTrans
import qualified Data.Array.Accelerate.Algebra.Ring                 as Ring
import qualified Data.Array.Accelerate.Algebra.Transcendental       as Trans
import qualified Data.Array.Accelerate.Algebra.ZeroTestable         as ZeroTestable

import Data.Array.Accelerate.Algebra.Absolute                       hiding ( C )
import Data.Array.Accelerate.Algebra.Additive                       hiding ( C )
import Data.Array.Accelerate.Algebra.Algebraic                      hiding ( C )
import Data.Array.Accelerate.Algebra.Field                          hiding ( C )
import Data.Array.Accelerate.Algebra.Ring                           hiding ( C )
import Data.Array.Accelerate.Algebra.Transcendental                 hiding ( C )
import Data.Array.Accelerate.Algebra.ZeroTestable                   hiding ( C )

import Crypto.Lol.Types.Unsafe.Complex                              ( Complex(..) )
import qualified Crypto.Lol.Types.Unsafe.Complex                    as C
import qualified Number.Complex                                     as NP

import Prelude                                                      ( ($), (.), undefined )


-- -- | Rounds the real and imaginary components to the nearest integral
-- --
-- roundComplex
--     :: forall a b. (RealRing.C (Exp a), ToInteger.C (Exp b), Elt a, Elt b)
--     => Exp (Complex a)
--     -> Exp (b, b)
-- roundComplex = lift1 (C.roundComplex :: Complex (Exp a) -> (Exp b, Exp b))

-- | 'cis' @t@ is a complex value with magnitude 1 and phase @t@ (modulo @2*pi@)
--
cis :: forall a. (Trans.C (Exp a), Elt a) => Exp a -> Exp (Complex a)
cis = lift1 (C.cis :: Exp a -> Complex (Exp a))

-- | Real component of a complex number
--
real :: forall a. Elt a => Exp (Complex a) -> Exp a
real = lift1 (C.real :: Complex (Exp a) -> Exp a)

-- | Imaginary component of a complex number
--
imag :: forall a. Elt a => Exp (Complex a) -> Exp a
imag = lift1 (C.imag :: Complex (Exp a) -> Exp a)

-- | Embeds a scalar as the real component of a complex number
--
fromReal :: (Additive.C (Exp a), Elt a) => Exp a -> Exp (Complex a)
fromReal = lift . C.fromReal

-- | Non-negative magnitude of a complex number
--
magnitude :: (Algebraic.C (Exp a), Elt a) => Exp (Complex a) -> Exp a
magnitude = sqrt . magnitudeSqr

magnitudeSqr :: (Ring.C (Exp a), Elt a) => Exp (Complex a) -> Exp a
magnitudeSqr c = (real c ^ 2) + (imag c ^ 2)

-- | Scale a complex number by a real number
--
scale :: (Ring.C (Exp a), Elt a) => Exp a -> Exp (Complex a) -> Exp (Complex a)
scale r c = lift $ Complex' (r * real c NP.+: r * imag c)


-- Helpers to convert between the Lol / numeric-prelude representation
--
unwrap :: Elt a => Exp (Complex a) -> Exp (Complex.T a)
unwrap z = real z Complex.+: imag z

wrap :: forall a. Elt a => Exp (Complex.T a) -> Exp (Complex a)
wrap z = lift $ Complex' (unlift z :: Complex.T (Exp a))

liftC :: forall a. Elt a => (Exp (Complex.T a) -> Exp (Complex.T a)) -> Exp (Complex a) -> Exp (Complex a)
liftC f = wrap . f . unwrap


-- It is really sad that I am defining this again from scratch, rather than
-- basing it off the implementation in numeric-prelude-accelerate.
--
type instance EltRepr (Complex a) = EltRepr (a, a)

instance Elt a => Elt (Complex a) where
  eltType _             = eltType (undefined :: (a,a))
  toElt p               = let (a, b) = toElt p in Complex' (a NP.+: b)
  fromElt (Complex' c)   = fromElt (NP.real c, NP.imag c)

instance cst a => IsProduct cst (Complex a) where
  type ProdRepr (Complex a) = ProdRepr (a, a)
  fromProd cst (Complex' c) = fromProd cst (NP.real c, NP.imag c)
  toProd cst p          = let (x, y) = toProd cst p in Complex' (x NP.+: y)
  prod cst _            = prod cst (undefined :: (a, a))

instance (Lift Exp a, Elt (Plain a)) => Lift Exp (Complex a) where
  type Plain (Complex a) = Complex (Plain a)
  lift (Complex' c)       = Exp . Tuple $ NilTup `SnocTup` lift (NP.real c)
                                                `SnocTup` lift (NP.imag c)

instance Elt a => Unlift Exp (Complex (Exp a)) where
  unlift e =
    let x = Exp $ SuccTupIdx ZeroTupIdx `Prj` e
        y = Exp $ ZeroTupIdx `Prj` e
    in
    Complex' (x NP.+: y)

instance A.Eq a => A.Eq (Complex a) where
  x == y = real x == real y && imag x == imag y
  x /= y = real x /= real y || imag x /= imag y

instance (ZeroTestable.C (Exp a), Elt a) => ZeroTestable.C (Exp (Complex a)) where
  isZero c = isZero (real c) && isZero (imag c)

instance (Additive.C (Exp a), Elt a) => Additive.C (Exp (Complex a)) where
  zero   = lift (Additive.zero :: Complex (Exp a))
  (+)    = lift2 ((+) :: Complex (Exp a) -> Complex (Exp a) -> Complex (Exp a))
  (-)    = lift2 ((-) :: Complex (Exp a) -> Complex (Exp a) -> Complex (Exp a))
  negate = lift1 (negate :: Complex (Exp a) -> Complex (Exp a))

instance (Ring.C (Exp a), Elt a) => Ring.C (Exp (Complex a)) where
  (*)           = lift2 ((*) :: Complex (Exp a) -> Complex (Exp a) -> Complex (Exp a))
  one           = lift (one :: Complex (Exp a))
  fromInteger x = lift (fromInteger x :: Complex (Exp a))

instance (Field.C (Exp a), Elt a) => Field.C (Exp (Complex a)) where
  (/)             = lift2 ((/) :: Complex (Exp a) -> Complex (Exp a) -> Complex (Exp a))
  fromRational' x = lift (fromRational' x :: Complex (Exp a))

instance (Absolute.C (Exp a), Algebraic.C (Exp a), ZeroTestable.C (Exp a), Elt a) => Absolute.C (Exp (Complex a)) where
  abs    = liftC abs
  signum = liftC signum

instance (Absolute.C (Exp a), Algebraic.C (Exp a), Field.C (Exp a), RealTrans.C (Exp a), Trans.C (Exp a), ZeroTestable.C (Exp a), A.Ord a, Elt a)
    => Algebraic.C (Exp (Complex a)) where
  sqrt   = liftC sqrt
  z ^/ r = liftC (^/ r) z

instance (Field.C (Exp a), RealTrans.C (Exp a), Trans.C (Exp a), ZeroTestable.C (Exp a), A.Ord a, Elt a)
    => Trans.C (Exp (Complex a)) where
  pi   = wrap pi
  exp  = liftC exp
  log  = liftC log
  sin  = liftC sin
  cos  = liftC cos
  sinh = liftC sinh
  cosh = liftC cosh
  asin = liftC asin
  acos = liftC acos
  atan = liftC atan

instance (A.FromIntegral a b, Additive.C (Exp b), Elt b) => A.FromIntegral a (Complex b) where
  fromIntegral = fromReal . A.fromIntegral

