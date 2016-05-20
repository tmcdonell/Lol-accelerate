{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE PolyKinds             #-}
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

import Data.Array.Accelerate                                        ( Exp, Elt, Z(..), (:.)(..) )
import qualified Data.Array.Accelerate                              as A

import Crypto.Lol.Cyclotomic.Tensor.Accelerate.Backend
import Crypto.Lol.Cyclotomic.Tensor.Accelerate.Common               hiding ( wrap, wrap2 )
import Crypto.Lol.Cyclotomic.Tensor.Representation
import qualified Crypto.Lol.Cyclotomic.Tensor.Accelerate.Common     as Arr

import Crypto.Lol.Types.IZipVector
import Crypto.Lol.Types.FiniteField
import Crypto.Lol.LatticePrelude                                    as P

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
import qualified Data.Vector.Generic                                as V

-- import Debug.Trace


-- | Tensor backed by Accelerate stages the computation as an expression
--
data AT (m :: Factored) r where
  AT :: Elt r => Arr m r -> AT m r
  ZV :: IZipVector m r -> AT m r

type instance TRep AT r = Exp r


-- Extra instances required to support implementation of 'Tensor' backed by
-- Accelerate.
--
-- Note that instances from 'Functor', 'Applicative', 'Foldable' and
-- 'Traversable' are computed via 'toZV' which requires an /O(n)/ conversion
-- step to a *boxed* vector representation, so should be avoided as much as
-- possible.
--

-- Standard instances

deriving instance Show r => Show (AT m r)

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
  traverse f a = traverse f (toZV a)


-- Numeric prelude instances

instance (Fact m, Additive (Exp r), Elt r) => Additive.C (AT m r) where
  zero   = AT $ repl zero
  (+)    = wrap2 (Arr.wrap2 (A.zipWith (+)))
  (-)    = wrap2 (Arr.wrap2 (A.zipWith (-)))
  negate = wrap  (Arr.wrap  (A.map negate))

instance (GFCtx fp d, Fact m, Additive (AT m fp)) => Module.C (GF fp d) (AT m fp) where
  --
  r *> (AT at)
    = let arr = run (unArr at)
          xs  = A.toList arr
          m   = r P.*> Coeffs xs
      in
      AT . Arr . A.use $ A.fromList (A.arrayShape arr) (unCoeffs m)
  --
  r *> (ZV zv)
    = let xs  = V.toList (unIZipVector zv)
          m   = r P.*> Coeffs xs
      in
      ZV . fromMaybe (error "AT.*>: internal error")
         $ iZipVector (V.fromList (unCoeffs m))

instance (Fact m, NPZT.C r, ZeroTestable.C (Exp r), Elt r) => NPZT.C (AT m r) where
  isZero (ZV v) = NPZT.isZero v
  isZero (AT a) =
    let r = A.all ZeroTestable.isZero (unArr a)
    in  A.indexArray (run r) Z


-- Miscellaneous instances

instance NFData r => NFData (AT m r) where
  rnf (AT !_) = ()      -- lies.
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
  -- trace "Lol.Accelerate.toAT" $ -- debugging
  AT . Arr $ A.use (A.fromFunction sh f)

-- | /O(n)/ Convert internal representation to IZipVector. Note that this
-- entails executing the Accelerate computation.
--
toZV :: Fact m => AT m r -> AT m r
toZV v@ZV{}         = v
toZV (AT (Arr acc)) =
  let
      arr    = run acc
      Z :. n = A.arrayShape arr
      f i    = arr `A.indexArray` (Z :. i)
  in
  -- trace "Lol.Accelerate.toZV" $ -- debugging
  ZV $ fromMaybe (error "Accelerate.toZV: internal error")
     $ iZipVector (V.generate n f)


wrap :: (Elt a, Elt b) => (Arr ma a -> Arr mb b) -> AT ma a -> AT mb b
wrap f = AT . unwrap f

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
  error "Why do you have a miniature block hole on your coffee table?"

