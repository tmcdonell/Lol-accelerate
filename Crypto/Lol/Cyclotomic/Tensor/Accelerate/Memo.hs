{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Crypto.Lol.Cyclotomic.Tensor.Accelerate.Memo
-- Copyright   : [2016] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- GHC isn't magic enough to automatically cache and reuse our invocations of
-- runN, which we require in order to bypass re-compilation on subsequent
-- invocations of these tensor functions at a given modulus/ring combination.
-- Thus, we must do it ourselves.
--
-- This is obviously not as good as it happening automagically (we have an
-- O(log n) lookup each time we want to call a function, rather than the
-- function being a top-level CAF which is invoked directly), but still better
-- than redoing the entire Accelerate pipeline.
--

module Crypto.Lol.Cyclotomic.Tensor.Accelerate.Memo
  where

import Control.Concurrent.MVar
import Data.HashMap.Strict                                          ( HashMap )
import Data.Hashable
import Data.Proxy
import Data.Typeable
import System.IO.Unsafe
import qualified Data.HashMap.Strict                                as Map

import Data.Array.Accelerate                                        ( Elt )

import Crypto.Lol.Prelude                                           ( Fact, proxy, ppsFact )


-- Memo tables over a single modulus
--
data ModRing m r where
  ModRing   :: (Fact m, Elt r)
            => {-# UNPACK #-} !(Proxy m)
            -> {-# UNPACK #-} !(Proxy r)
            -> ModRing m r

instance Eq (ModRing m r) where
  ModRing m1 r1 == ModRing m2 r2
    = proxy ppsFact m1 == proxy ppsFact m2 && typeRep r1 == typeRep r2

instance Hashable (ModRing m r) where
  hashWithSalt salt (ModRing m1 r1)
    = salt `hashWithSalt` proxy ppsFact m1 `hashWithSalt` typeRep r1


type MemoTable m r a = MVar ( HashMap (ModRing m r) a )

newMemoTable :: IO (MemoTable m r a )
newMemoTable = newMVar ( Map.empty )

memo :: forall m r a. (Fact m, Elt r)
     => Proxy m
     -> Proxy r
     -> MemoTable m r a
     -> a
     -> a
memo _ _ !ref ~val =
  let key = ModRing (Proxy::Proxy m) (Proxy::Proxy r)
  in  memo' ref key val


-- Memo tables over two modulii (ugh...)
--

data Mod2Ring m1 m2 r where
  Mod2Ring  :: (Fact m1, Fact m2, Elt r)
            => {-# UNPACK #-} !(Proxy m1)
            -> {-# UNPACK #-} !(Proxy m2)
            -> {-# UNPACK #-} !(Proxy r)
            -> Mod2Ring m1 m2 r

instance Eq (Mod2Ring m1 m2 r) where
  Mod2Ring m11 m12 r1 == Mod2Ring m21 m22 r2
    = proxy ppsFact m11 == proxy ppsFact m21
   && proxy ppsFact m12 == proxy ppsFact m22
   && typeRep r1 == typeRep r2

instance Hashable (Mod2Ring m1 m2 r) where
  hashWithSalt salt (Mod2Ring m1 m2 r1)
    = salt `hashWithSalt` proxy ppsFact m1
           `hashWithSalt` proxy ppsFact m2
           `hashWithSalt` typeRep r1


type MemoTable2 m1 m2 r a = MVar ( HashMap (Mod2Ring m1 m2 r) a )

newMemoTable2 :: IO (MemoTable2 m1 m2 r a )
newMemoTable2 = newMVar ( Map.empty )

memo2 :: forall m1 m2 r a. (Fact m1, Fact m2, Elt r)
      => Proxy m1
      -> Proxy m2
      -> Proxy r
      -> MemoTable2 m1 m2 r a
      -> a
      -> a
memo2 _ _ _ !ref ~val =
  let key = Mod2Ring (Proxy::Proxy m1) (Proxy::Proxy m2) (Proxy::Proxy r)
  in  memo' ref key val

memo' :: (Eq k, Hashable k)
      => MVar (HashMap k v)
      -> k
      -> v
      -> v
memo' !ref !key ~val
  = unsafePerformIO
  $ modifyMVar ref $ \table ->
      case Map.lookup key table of
        Nothing  -> return (Map.insert key val table, val)
        Just old -> return (table, old)

