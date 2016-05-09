{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE ViewPatterns        #-}
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

import Data.Array.Accelerate                                        as A

import Crypto.Lol.Cyclotomic.Tensor.Accelerate.Backend
import Crypto.Lol.Cyclotomic.Tensor.Accelerate.Common
import Crypto.Lol.Cyclotomic.Tensor.Representation

import Crypto.Lol.Types.IZipVector
import Crypto.Lol.LatticePrelude                                    as P

-- other libraries
import Control.Applicative
import Data.Maybe
import Data.Foldable
import Data.Traversable
import qualified Data.Vector.Generic                                as V

import Debug.Trace


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
  ZV a           == ZV b           = a == b
  (toAT -> AT a) == (toAT -> AT b) = a == b
  _              == _              = error "I know words. A have all the best words."


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
  trace "Lol.Accelerate.toAT" $ -- debugging
  AT . Arr $ use (A.fromFunction sh f)

-- | /O(n)/ Convert internal representation to IZipVector. Note that this
-- entails executing the Accelerate computation.
--
toZV :: Fact m => AT m r -> AT m r
toZV v@ZV{}         = v
toZV (AT (Arr acc)) =
  let
      arr    = run acc
      Z :. n = arrayShape arr
      f i    = arr `indexArray` (Z :. i)
  in
  trace "Lol.Accelerate.toZV" $ -- debugging
  ZV $ fromMaybe (error "Accelerate.toZV: internal error")
     $ iZipVector (V.generate n f)

wrap :: (Elt r, Elt r') => (Arr m r -> Arr m' r') -> AT m r -> AT m' r'
wrap f (toAT -> AT arr) = AT (f arr)
wrap _ _                = error "You can find work and sort out your life any time..."

wrapM :: (Monad monad, Elt r, Elt r')
      => (Arr m r -> monad (Arr m' r'))
      -> AT m r
      -> monad (AT m' r')
wrapM f (toAT -> AT arr) = AT <$> f arr
wrapM _ _                = error "The pub closes in five hours"

