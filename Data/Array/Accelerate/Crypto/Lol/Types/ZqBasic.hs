{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Array.Accelerate.Crypto.Lol.Types.ZqBasic () -- only instances
  where

import Data.Array.Accelerate                                        as A hiding ( (-) )
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

import Crypto.Lol.Cyclotomic.Tensor.Accelerate.AT

import Crypto.Lol.CRTrans
import Crypto.Lol.LatticePrelude                                    as LP hiding ( modinv )
import Crypto.Lol.Reflects
import Crypto.Lol.Types.FiniteField
import Crypto.Lol.Types.ZPP
import Crypto.Lol.Types.ZqBasic

import qualified Algebra.ZeroTestable                               as NPZT

import Math.NumberTheory.Primes                                     ( factorise, isPrime )

import Control.Applicative                                          ( (<$>), (<*>) )
import Data.Typeable
import Unsafe.Coerce


-- CRTrans instances
-- -----------------

instance (ReflectsTI q z, Ring (Exp (ZqBasic q z)), FromIntegral z Double, Typeable (ZqBasic q))
    => CRTEmbed AT (ZqBasic q z) where
  type CRTExt (ZqBasic q z) = Complex Double
  --
  toExt   = tag $ \x -> let ZqB z = unliftZq x
                        in  A.fromReal (A.fromIntegral z)
  fromExt = tag $ reduce' . A.round . A.real


instance (ReflectsTI q z, Ring (Exp z), PID (Exp z), ToInteger z, Enumerable (ZqBasic q z), FromIntegral Int z, Typeable (ZqBasic q))
    => CRTrans Maybe (Exp Int) (Exp (ZqBasic q z)) where
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
    :: forall m q z. ( ReflectsTI q z, Ring z, ToInteger z, Enumerable (ZqBasic q z)
                     , Reflects m Int, Ring (Exp z), Typeable (ZqBasic q) )
    => TaggedT m Maybe (Exp Int -> Exp (ZqBasic q z))
principalRootOfUnity =
  let
      q        = LP.fromIntegral (proxy value (Proxy::Proxy q) :: z)    -- use Integer for intermediates
      mval     = proxy value (Proxy::Proxy m)
      order    = q-1                                                    -- order is Zq^* (assuming q is prime)
      pfactors = fmap LP.fst (factorise order)                          -- the primes dividing the order of Zq^*
      exps     = fmap (LP.div order) pfactors                           -- powers we need to check
      isGen x  = x^order == one && LP.all (\e -> x^e /= one) exps       -- whether an element is a generator of Zq^*
      (mq,mr)  = order `LP.divMod` LP.fromIntegral mval
      omega    = head (LP.filter isGen values) ^ mq :: ZqBasic q z
  in
  tagT $ if isPrime q && mr == 0
            then Just $ \i -> A.iterate (i `LP.mod` constant mval) (LP.* constant omega) one -- omega ** (i `mod` m) ??
            else Nothing

mhatInv
    :: forall m q z. ( ReflectsTI q z, Reflects m Int, PID (Exp z)
                     , A.Num z, A.FromIntegral Int z, Elt z, Typeable (ZqBasic q)
                     )
    => TaggedT m Maybe (Exp (ZqBasic q z))
mhatInv =
  let q    = constant $ proxy value (Proxy::Proxy q)  :: Exp z
      mval = constant $ proxy value (Proxy::Proxy m)  :: Exp Int
      mhat = let (d,m) = LP.divMod mval 2 -- this is @valueHat mval@ lifted to Exp
             in  m ==* 0 ? ( d, mval )
  in
  return $ reduce' (A.fromIntegral mhat `modinv` q)


-- ZPP instance
-- ------------

instance ( PPow pp, zq ~ ZqBasic pp z, PrimeField (ZpOf zq), Ring zq, Ring (ZpOf zq), ToInteger z
         , Ring (Exp z), A.Integral z, A.IsIntegral z, Typeable pp)
    => ZPP AT (ZqBasic (pp :: PrimePower) z) where
  type ZpOf (ZqBasic pp z) = ZqBasic (PrimePP pp) z
  --
  modulusZPP = retag (ppPPow :: Tagged pp PP)
  liftZp     = tag unsafeCoerce -- Data.Coerce.coerce didn't work, so use a bigger hammer...


-- Numeric prelude instances
-- -------------------------

instance (ReflectsTI q z, Additive (Exp z), Typeable (ZqBasic q)) => Additive.C (Exp (ZqBasic q z)) where
  zero  = A.lift (ZqB (zero :: Exp z))
  x + y = let
              q      = constant $ proxy value (Proxy::Proxy q)
              ZqB x' = unliftZq x
              ZqB y' = unliftZq y
              z      = x' LP.+ y'
          in
          A.lift $ ZqB (z >=* q ? ( z LP.- q, z ))
  --
  negate x = let ZqB z = unliftZq x
             in  reduce' (LP.negate z)


instance (ReflectsTI q z, ToInteger z, Ring (Exp z), Typeable (ZqBasic q)) => Ring.C (Exp (ZqBasic q z)) where
  x * y = let
              ZqB x' = unliftZq x
              ZqB y' = unliftZq y
          in
          reduce' (x' LP.* y')
  --
  fromInteger = constant . fromInteger


instance (ReflectsTI q z, ToInteger z, PID (Exp z), Typeable (ZqBasic q))
    => Field.C (Exp (ZqBasic q z)) where
  recip x = let
                q     = constant $ proxy value (Proxy::Proxy q)
                ZqB z = unliftZq x
            in
            A.lift (ZqB (z `modinv` q))

instance (Field (Exp (ZqBasic q z)), Typeable (ZqBasic q)) => IntegralDomain.C (Exp (ZqBasic q z)) where
  divMod a b = (a LP./ b, zero)

instance (ZeroTestable.C (Exp z), Elt z, Typeable (ZqBasic q)) => ZeroTestable.C (Exp (ZqBasic q z)) where
  isZero (unliftZq -> ZqB z) = ZeroTestable.isZero z

instance NPZT.C (Exp z) where
  isZero = error "numeric-prelude error: use Data.Array.Accelerate.Algebra.ZeroTestable.isZero instead"


-- Accelerate instances
-- --------------------

instance (A.Eq z, Typeable (ZqBasic q)) => A.Eq (ZqBasic q z) where
  (unliftZq -> ZqB x) ==* (unliftZq -> ZqB y) = x ==* y
  (unliftZq -> ZqB x) /=* (unliftZq -> ZqB y) = x /=* y

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


-- | Inverse of @a@ modulo @q@, in range @[0..q-1]@.
--
-- XXX: How do we signal error from Accelerate?
--
modinv :: (PID (Exp i), A.Eq i) => Exp i -> Exp i -> Exp i
modinv a q =
  let (d, (_, inv)) = extendedGCD q a
  in  d ==* one ? ( inv `LP.mod` q, {- TLM: error!!?1 -} zero )


unliftZq :: (Elt z, Typeable (ZqBasic q)) => Exp (ZqBasic q z) -> ZqBasic q (Exp z)
unliftZq z = ZqB (Exp $ ZeroTupIdx `Prj` z)


-- Lifting ZqBasic into Accelerate
-- -------------------------------

-- Bundling 'z' in a tuple with unit in order to define an 'IsProduct' instance,
-- and from there a 'Lift'. This still doesn't let us define 'Unlift', however
-- (see below).
--
type instance EltRepr (ZqBasic q z) = ((), EltRepr z)

instance (Elt z, Typeable (ZqBasic q)) => Elt (ZqBasic q z) where
  eltType _       = PairTuple UnitTuple (eltType (undefined::z))
  toElt ((),z)    = ZqB (toElt z)
  fromElt (ZqB z) = ((), fromElt z)

instance Elt z => IsProduct Elt (ZqBasic q z) where
  type ProdRepr (ZqBasic q z) = ProdRepr ((), z)
  toProd _ t         = let ((), z) = toTuple t in ZqB z
  fromProd _ (ZqB z) = fromTuple ((), z)
  prod cst _         = prod cst (undefined :: ((), z))

instance (A.Lift Exp z, Elt (Plain z), Typeable (ZqBasic q)) => A.Lift Exp (ZqBasic q z) where
  type Plain (ZqBasic q z) = ZqBasic q (Plain z)
  --
  lift (ZqB z) = Exp . Tuple $ NilTup `SnocTup` A.lift ()
                                      `SnocTup` A.lift z

-- Can't define an 'Unlift' instance because the type expected by unlift is not
-- what we wanted (TLM: why?). Instead, define our own 'unliftZq'.
--
-- instance (A.Unlift Exp z, Elt (Plain z), Typeable (ZqBasic q)) => A.Unlift Exp (ZqBasic q z) where
--   unlift :: Exp (Plain (ZqBasic q z)) -> ZqBasic q z ~~ !!

