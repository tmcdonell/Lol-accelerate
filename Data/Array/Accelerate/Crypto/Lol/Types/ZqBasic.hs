{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}

module Data.Array.Accelerate.Crypto.Lol.Types.ZqBasic () -- only instances
  where

import Data.Array.Accelerate                                        as A hiding ( (-), (*) )
import Data.Array.Accelerate.Array.Sugar                            as A
import Data.Array.Accelerate.Smart                                  as A
import Data.Array.Accelerate.Type                                   as A
import Data.Array.Accelerate.Product                                as A

import qualified Data.Array.Accelerate.Algebra.Additive             as Additive
import qualified Data.Array.Accelerate.Algebra.Field                as Field
import qualified Data.Array.Accelerate.Algebra.IntegralDomain       as IntegralDomain
import qualified Data.Array.Accelerate.Algebra.PrincipalIdealDomain as PID ()
import qualified Data.Array.Accelerate.Algebra.Ring                 as Ring
import qualified Data.Array.Accelerate.Algebra.ZeroTestable         as ZeroTestable

import Data.Array.Accelerate.Crypto.Lol.Types.Complex               as A

import Crypto.Lol.CRTrans
import Crypto.Lol.Prelude                                           as LP hiding ( modinv, FromIntegral )
import Crypto.Lol.Reflects
import Crypto.Lol.Types.FiniteField
import Crypto.Lol.Types.ZPP
import Crypto.Lol.Types.Unsafe.ZqBasic

import qualified Algebra.ZeroTestable                               as NPZT

import Math.NumberTheory.Primes                                     ( factorise, isPrime )

import Control.Applicative                                          ( (<$>), (<*>) )
import Data.Typeable


-- CRTrans instances
-- -----------------

instance (ReflectsTI q z, Ring (Exp (ZqBasic q z)), FromIntegral z Double, Typeable (ZqBasic q))
    => CRTEmbed (Exp (ZqBasic q z)) where
  type CRTExt (Exp (ZqBasic q z)) = Exp (Complex Double)
  --
  toExt x = let ZqB z = unlift x
            in  A.fromReal (A.fromIntegral z)
  fromExt = reduce' . A.round . A.real


instance (ReflectsTI q z, Ring (Exp z), PID z, ToInteger z,
          Enumerable (ZqBasic q z), Typeable (ZqBasic q))
    => CRTrans Maybe (Exp (ZqBasic q z)) where
  type CRTIndex (Exp (ZqBasic q z)) = Exp Int
  crtInfo = (,) <$> principalRootOfUnity
                <*> mhatInv

-- | Yield the /principal/ @m@th root of unity @omega_m@ in @Z_q^*@. The
-- implementation requires @q@ to be prime. It works by finding a generator in
-- @Z_q^*@ and raising it to the @(q-1)/m@ power.
--
-- Note that we make heavy use of metaprogramming to compute the value omega,
-- which otherwise would be quite difficult to obtain.
--
principalRootOfUnity
    :: forall m q z . ( ReflectsTI q z, ToInteger z, Enumerable (ZqBasic q z)
                     , Reflects m Int, Ring (Exp z), Typeable (ZqBasic q) )
    => TaggedT m Maybe (Exp Int -> Exp (ZqBasic q z))
principalRootOfUnity =
  let
      q        = LP.fromIntegral (proxy value (Proxy::Proxy q) :: z)                  -- use Integer for intermediates
      mval     = proxy value (Proxy::Proxy m) :: Int
      order    = q-1                                                                  -- order is Zq^* (assuming q is prime)
      pfactors = LP.fst <$> factorise order                                           -- the primes dividing the order of Zq^*
      exps     = LP.div order <$> pfactors                                            -- powers we need to check
      isGen x  = x LP.^ order LP.== one LP.&& LP.all (\e -> x LP.^ e LP./= one) exps  -- whether an element is a generator of Zq^*
      (mq,mr)  = order `LP.divMod` LP.fromIntegral mval
      omega    = head (LP.filter isGen values) LP.^ mq
  in
  tagT $ if isPrime q LP.&& mr LP.== 0
            then Just $ \i -> A.iterate (i `LP.mod` constant mval) (LP.* constant omega) one -- omega ** (i `mod` m) ??
            else Nothing

mhatInv
    :: forall m q z. (CRTrans Maybe (ZqBasic q z), Reflects m Int, Elt z, Typeable (ZqBasic q))
    => TaggedT m Maybe (Exp (ZqBasic q z))
mhatInv =
  constant . LP.snd <$> (crtInfo :: TaggedT m Maybe (CRTInfo (ZqBasic q z)))

--constant :: i -> Exp i
-- ZPP instance
-- ------------

type instance CharOf (Exp (ZqBasic q z)) = q

instance ( PPow pp, zq ~ Exp (ZqBasic pp z), PrimeField (ZpOf zq)
         , Ring zq
         , Elt z, Typeable pp , Typeable (ZqBasic (PrimePP pp))
         )
    => ZPP (Exp (ZqBasic (pp :: PrimePower) z)) where
  --
  type ZpOf (Exp (ZqBasic pp z)) = Exp (ZqBasic (PrimePP pp) z)
  --
  modulusZPP = retag (ppPPow :: Tagged pp PP)
  liftZp x   = let ZqB z = unlift x :: ZqBasic (PrimePP pp) (Exp z)
               in  A.lift (ZqB z)


-- Lattice prelude instances
-- -------------------------

type instance LiftOf (Exp (ZqBasic p z)) = Exp z

instance (ReflectsTI q z, Ring (Exp z), Typeable (ZqBasic q)) => Lift' (Exp (ZqBasic q z)) where
  lift = decode'

instance (ReflectsTI q z, Additive (Exp z), Typeable (ZqBasic q)) => Reduce (Exp z) (Exp (ZqBasic q z)) where
  reduce = reduce'

instance (ReflectsTI q z, ToInteger z, Enum z, Typeable (ZqBasic q)) => Enumerable (Exp (ZqBasic q z)) where
  values = LP.map A.constant values


-- Numeric prelude instances
-- -------------------------

instance (ReflectsTI q z, Additive (Exp z), Typeable (ZqBasic q)) => Additive.C (Exp (ZqBasic q z)) where
  zero  = A.lift (ZqB (zero :: Exp z))
  x + y = let
              q      = constant $ proxy value (Proxy::Proxy q)
              ZqB x' = unlift x
              ZqB y' = unlift y
              z      = x' LP.+ y'
          in
          A.lift $ ZqB (z A.>= q ? ( z LP.- q, z ))
  --
  negate x = let ZqB z = unlift x
             in  reduce' (LP.negate z)


instance (ReflectsTI q z, ToInteger z, Ring (Exp z), Typeable (ZqBasic q)) => Ring.C (Exp (ZqBasic q z)) where
  x * y = let
              ZqB x' = unlift x
              ZqB y' = unlift y
          in
          reduce' (x' LP.* y')
  --
  fromInteger = constant . LP.fromInteger


instance (ReflectsTI q z, ToInteger z, PID (Exp z), Typeable (ZqBasic q))
    => Field.C (Exp (ZqBasic q z)) where
  recip x = let
                q     = constant $ proxy value (Proxy::Proxy q)
                ZqB z = unlift x
            in
            A.lift (ZqB (z `modinv` q))

instance (Field (Exp (ZqBasic q z)), Typeable (ZqBasic q), ReflectsTI q z, ToInteger z, Ring (Exp z)) => IntegralDomain.C (Exp (ZqBasic q z)) where
  divMod a b = (a LP./ b, zero)

instance (ZeroTestable.C (Exp z), Elt z, Typeable (ZqBasic q)) => ZeroTestable.C (Exp (ZqBasic q z)) where
  isZero (unlift -> ZqB z :: ZqBasic q (Exp z)) = ZeroTestable.isZero z

instance NPZT.C (Exp (ZqBasic q z)) where
  isZero = error "numeric-prelude error: use Data.Array.Accelerate.Algebra.ZeroTestable.isZero instead"


-- Accelerate instances
-- --------------------

instance (A.Eq z, Typeable (ZqBasic q)) => A.Eq (ZqBasic q z) where
  (==) = lift2 (A.==)
  (/=) = lift2 (A./=)

instance (A.FromIntegral a z, Elt z, Typeable (ZqBasic q)) => A.FromIntegral a (ZqBasic q z) where
  fromIntegral = A.lift . ZqB . A.fromIntegral

-- instance (Additive.C (Exp (ZqBasic q z)), Ring.C (Exp (ZqBasic q z)), Field.C (Exp (ZqBasic q z)), Typeable (ZqBasic q))
--     => P.Num (Exp (ZqBasic q z)) where
--   (+)         = (Additive.+)
--   (-)         = (Additive.-)
--   (*)         = (Ring.*)
--   negate      = Additive.negate
--   fromInteger = Ring.fromInteger


-- Utilities
-- ---------

type ReflectsTI q z = (Reflects q z, A.Integral z, A.IsIntegral z)

reduce'
    :: forall q z. (ReflectsTI q z, Typeable (ZqBasic q))
    => Exp z
    -> Exp (ZqBasic q z)
reduce' x = A.lift (ZqB z)
  where
    z = x `A.mod` constant (proxy value (Proxy::Proxy q))

-- puts the value in the range [-q/2, q/2]
--
decode'
    :: forall q z. (ReflectsTI q z, Ring (Exp z), Typeable (ZqBasic q))
    => Exp (ZqBasic q z)
    -> Exp z
decode' x =
  let qval  = proxy value (Proxy::Proxy q)
      ZqB z = unlift x
  in
  2 * z A.< constant qval ? ( z, z - constant qval )


-- | Inverse of @a@ modulo @q@, in range @[0..q-1]@.
--
-- XXX: How do we signal error from Accelerate?
--
modinv :: (PID (Exp i), A.Eq i) => Exp i -> Exp i -> Exp i
modinv a q =
  let (d, (_, inv)) = extendedGCD q a
  in  d A.== one ? ( inv `LP.mod` q, {- TLM: error!!?1 -} zero )


-- Lifting ZqBasic into Accelerate
-- -------------------------------

-- Bundling 'z' in a tuple with unit in order to distinguish it from plain 'z'.
--
type instance EltRepr (ZqBasic q z) = ((), EltRepr z)

instance (Elt z, Typeable (ZqBasic q)) => Elt (ZqBasic q z) where
  eltType _       = PairTuple UnitTuple (eltType (undefined::z))
  toElt ((),z)    = ZqB (toElt z)
  fromElt (ZqB z) = ((), fromElt z)

instance Elt z => IsProduct Elt (ZqBasic q z) where
  type ProdRepr (ZqBasic q z) = ((), z)
  toProd _ ((),z)    = ZqB z
  fromProd _ (ZqB z) = ((), z)
  prod _ _           = ProdRsnoc ProdRunit

instance (A.Lift Exp z, Elt (Plain z), Typeable (ZqBasic q)) => A.Lift Exp (ZqBasic q z) where
  type Plain (ZqBasic q z) = ZqBasic q (Plain z)
  lift (ZqB z) = Exp . Tuple $ NilTup `SnocTup` A.lift z

instance (Elt z, Typeable (ZqBasic q)) => A.Unlift Exp (ZqBasic q (Exp z)) where
  unlift z = ZqB . Exp $ ZeroTupIdx `Prj` z

