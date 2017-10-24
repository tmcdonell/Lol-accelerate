{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}
-- |
-- Module      : Crypto.Lol.Cyclotomic.Tensor.Accelerate.AT
-- Copyright   : [2016] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Representation of Accelerate-backed tensors
--

module Crypto.Lol.Cyclotomic.Tensor.Accelerate.AT
  where

import Data.Array.Accelerate                                        ( Exp, Elt, Scalar, Vector, Z(..), (:.)(..), All(..) )
import qualified Data.Array.Accelerate                              as A

import Crypto.Lol.Cyclotomic.Tensor.Accelerate.Backend
import Crypto.Lol.Cyclotomic.Tensor.Accelerate.Memo
import Crypto.Lol.Cyclotomic.Tensor.Accelerate.Common               hiding ( wrap, wrap2 )
import qualified Crypto.Lol.Cyclotomic.Tensor.Accelerate.Common     as Arr

import Crypto.Lol.Prelude                                           as P
import Crypto.Lol.Reflects
import Crypto.Lol.Types.FiniteField                                 as FF
import Crypto.Lol.Types.IZipVector
import Crypto.Lol.Tests

import Data.Array.Accelerate.Algebra.Additive                       as Additive
import Data.Array.Accelerate.Algebra.Module                         as Module
import Data.Array.Accelerate.Algebra.ZeroTestable                   as ZeroTestable
import qualified Algebra.ZeroTestable                               as NPZT

-- other libraries
import Control.Applicative
import Control.DeepSeq
import Control.Monad.Random
import Data.Foldable
import Data.Maybe
import Data.Traversable
import System.IO.Unsafe
import qualified Data.Vector.Generic                                as V


-- | Tensor backed by Accelerate stages the computation as an expression
--
data AT (m :: Factored) r where
  AT :: Elt r => Arr m r -> AT m r
  ZV :: IZipVector m r -> AT m r

instance Show (ArgType AT) where
  show _ = "AT"

--type instance TRep AT r = Exp r

-- Extra instances required to support implementation of 'Tensor' backed by
-- Accelerate.
--
-- Note that instances from 'Functor', 'Applicative', 'Foldable' and
-- 'Traversable' are computed via 'toZV' which requires an /O(n)/ conversion
-- step to a *boxed* vector representation, so should be avoided as much as
-- possible.
--

-- Standard instances

instance Show r => Show (AT m r) where
  show (ZV z) = show (unIZipVector z)
  show (AT a) = show (unArr a)

instance (P.Eq r, A.Eq r) => P.Eq (AT m r) where
  ZV a == ZV b = a == b
  xs   == ys   = unwrap2 (==) xs ys


-- Category-theoretic instances

instance Fact m => Functor (AT m) where
  fmap f x = pure f <*> x

instance Fact m => Applicative (AT m) where
  pure                    = ZV . pure
  ZV f <*> (toZV -> ZV v) = ZV (f <*> v)
  _    <*> _              = error "(<*>): AT can never hold an (a -> b)"

instance Fact m => Foldable (AT m) where
  foldMap = foldMapDefault

instance Fact m => Traversable (AT m) where
  traverse f r@AT{} = traverse f (toZV r)
  traverse f (ZV v) = ZV <$> traverse f v


-- Numeric prelude instances

instance (Fact m, Additive (Exp r), Elt r) => Additive.C (AT m r) where
  zero   = AT $ repl zero
  (+)    = wrap2 (Arr.wrap2 (memo __additive_add (MK::MemoKey '(m,r)) $ runN (A.zipWith (+))))
  (-)    = wrap2 (Arr.wrap2 (memo __additive_sub (MK::MemoKey '(m,r)) $ runN (A.zipWith (-))))
  negate = wrap  (Arr.wrap  (memo __additive_neg (MK::MemoKey '(m,r)) $ runN (A.map negate)))

instance (GFCtx fp d, Fact m, Additive (AT m fp), Ring (Exp fp)) => Module.C (GF fp d) (AT m fp) where
  --
  r *> (AT (Arr at)) =
    let
        d   = proxy value (Proxy::Proxy d)
        gf  = A.fromList (Z :. d) (FF.toList r)
        --
        -- TLM: Instead of the 1D->2D->1D process we have here, we instead
        -- flatten the replicated gf array and perform the zipWith on the input
        -- array unchanged. This may produce more efficient code, but may also
        -- be less obvious what is going on?
        --
        -- AT . Arr $ A.reshape (A.index1 n)
        --          $ A.zipWith (*) (A.replicate (A.lift (Z :. h :. All)) gf)  -- All == d
        --                          (A.reshape   (A.lift (Z :. h :. d  )) at)
        --
        -- AT . Arr $ A.zipWith (*) (A.flatten (A.replicate (A.lift (Z :. h :. All)) gf)) at
        --
        scale g a =
          let n = A.length a
              h = n `div` A.constant d  -- error if n `mod` d /= 0, so later reshape is fine
          in A.reshape (A.index1 n)
           $ A.zipWith (*) (A.replicate (A.lift (Z :. h :. All)) g)  -- All == d
                           (A.reshape   (A.lift (Z :. h :. d  )) a)

        go = memo __module_scale (MK::MemoKey '(m,fp))
           $ runN scale
    in
    AT . Arr $! go gf at
  --
  r *> (ZV zv) =
    let xs  = V.toList (unIZipVector zv)
        m   = r P.*> Coeffs xs
    in
    ZV . fromMaybe (error "AT.*>: internal error")
       $ iZipVector (V.fromList (unCoeffs m))

instance (NPZT.C r, ZeroTestable.C (Exp r), Elt r) => NPZT.C (AT m r) where
  isZero (ZV v) = NPZT.isZero v
  isZero (AT a) = A.indexArray (go (unArr a)) Z
    where
      go = memo __iszero (MK::MemoKey r)
         $ runN (A.all ZeroTestable.isZero)


-- Miscellaneous instances

instance NFData r => NFData (AT m r) where
  rnf (AT at) = rnf (unArr at)
  rnf (ZV zv) = rnf zv

instance (Elt r, Random r, Fact m) => Random (AT m r) where
  random  = runRand $ AT <$> liftRand random
  randomR = error "AT.randomR: not supported"


-- Conversions
-- -----------
--
-- Avoid converting between representations as much as possible.

-- | /O(n)/ Convert internal representation to Accelerate
--
toAT :: Elt r => AT m r -> AT m r
toAT a@AT{} = a
toAT (ZV z) =
  let
      v          = unIZipVector z
      sh         = Z :. V.length v
      f (Z :. i) = v V.! i
  in
  -- Debug.trace "Lol.Accelerate.toAT" $ -- debugging
  AT $ Arr (A.fromFunction sh f)

-- | /O(n)/ Convert internal representation to IZipVector. Note that this
-- entails executing the Accelerate computation.
--
toZV :: Fact m => AT m r -> AT m r
toZV v@ZV{}         = v
toZV (AT (Arr arr)) =
  let
      Z :. n = A.arrayShape arr
      f i    = arr `A.indexArray` (Z :. i)
  in
  -- Debug.trace "Lol.Accelerate.toZV" $ -- debugging
  ZV $ fromMaybe (error "Accelerate.toZV: internal error")
     $ iZipVector (V.generate n f)


{-# INLINE wrap #-}
wrap :: (Elt a, Elt b) => (Arr ma a -> Arr mb b) -> AT ma a -> AT mb b
wrap f = AT . unwrap f

{-# INLINE wrap2 #-}
wrap2 :: (Elt a, Elt b, Elt c)
      => (Arr ma a -> Arr mb b -> Arr mc c)
      -> AT ma a
      -> AT mb b
      -> AT mc c
wrap2 f xs = AT . unwrap2 f xs

unwrap :: Elt a => (Arr m a -> b) -> AT m a -> b
unwrap f (toAT -> AT arr) = f arr
unwrap _ _ =
  error "You can find work and sort out your life any time, the pub closes in five hours."

unwrap2 :: (Elt a, Elt b)
        => (Arr ma a -> Arr mb b -> c)
        -> AT ma a
        -> AT mb b
        -> c
unwrap2 f (toAT -> AT xs) (toAT -> AT ys) = f xs ys
unwrap2 _ _ _ =
  error "Did you know the word 'recursion' contains the word 'recursion' _in itself_?!!"

wrapM :: (Monad monad, Elt r, Elt r')
      => (Arr m r -> monad (Arr m' r'))
      -> AT m r
      -> monad (AT m' r')
wrapM f (toAT -> AT arr) = AT <$> f arr
wrapM _ _ =
  error "Why do you have a miniature black hole on your coffee table?"


-- Memo tables
-- -----------

__additive_add, __additive_sub :: MemoTable '(m,r) (Vector r -> Vector r -> Vector r)
__additive_add = unsafePerformIO newMemoTable
__additive_sub = unsafePerformIO newMemoTable

__additive_neg :: MemoTable '(m,r) (Vector r -> Vector r)
__additive_neg = unsafePerformIO newMemoTable

__module_scale :: MemoTable '(m,r) (Vector r -> Vector r -> Vector r)
__module_scale = unsafePerformIO newMemoTable

__iszero :: MemoTable r (Vector r -> Scalar Bool)
__iszero = unsafePerformIO newMemoTable

