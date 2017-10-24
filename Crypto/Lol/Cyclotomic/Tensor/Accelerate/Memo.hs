{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PolyKinds           #-}
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

import Crypto.Lol.Prelude                                           ( Fact, proxy, valueFact )


type MemoTable k v = MVar ( HashMap (MemoKey k) v )

newMemoTable :: IO (MemoTable k v)
newMemoTable = newMVar ( Map.empty )

memo :: forall k v. (Eq (MemoKey k), Hashable (MemoKey k))
     => MemoTable k v
     -> MemoKey k
     -> v
     -> v
memo !ref _ val
  = unsafePerformIO
  $ modifyMVar ref $ \table ->
      case Map.lookup MK table of
        Nothing  -> return (Map.insert MK val table, val)
        Just old -> return (table, old)


data MemoKey (a :: k) = MK

instance Fact m => Hashable (MemoKey m) where
  hashWithSalt salt _ = salt `hashWithSalt` (proxy valueFact (Proxy::Proxy m))

instance Elt e => Hashable (MemoKey e) where
  hashWithSalt salt _ = salt `hashWithSalt` typeRep (Proxy::Proxy e)

instance (Hashable (MemoKey a), Hashable (MemoKey b)) => Hashable (MemoKey '(a,b)) where
  hashWithSalt salt _ = salt `hashWithSalt` (MK :: MemoKey a)
                             `hashWithSalt` (MK :: MemoKey b)

instance (Hashable (MemoKey a), Hashable (MemoKey b), Hashable (MemoKey c)) => Hashable (MemoKey '(a,b,c)) where
  hashWithSalt salt _ = salt `hashWithSalt` (MK :: MemoKey a)
                             `hashWithSalt` (MK :: MemoKey b)
                             `hashWithSalt` (MK :: MemoKey c)

instance Fact m => Show (MemoKey m) where
  show _ = 'F' : show (proxy valueFact (Proxy::Proxy m))

instance Elt e => Show (MemoKey e) where
  show _ = showsTypeRep (typeRep (Proxy::Proxy e)) ""

instance (Show (MemoKey a), Show (MemoKey b)) => Show (MemoKey '(a,b)) where
  show _ = "(" ++ show (MK::MemoKey a) ++ "," ++ show (MK::MemoKey b) ++ ")"

instance (Show (MemoKey a), Show (MemoKey b), Show (MemoKey c)) => Show (MemoKey '(a,b,c)) where
  show _ = "(" ++ show (MK::MemoKey a) ++ "," ++ show (MK::MemoKey b) ++ "," ++ show (MK::MemoKey c) ++ ")"

instance Fact m => Eq (MemoKey m) where
  _ == _ = True
  _ /= _ = False

instance Elt e => Eq (MemoKey e) where
  _ == _ = True
  _ /= _ = False

instance (Eq (MemoKey a), Eq (MemoKey b)) => Eq (MemoKey '(a,b)) where
  _ == _ = True
  _ /= _ = False

instance (Eq (MemoKey a), Eq (MemoKey b), Eq (MemoKey c)) => Eq (MemoKey '(a,b,c)) where
  _ == _ = True
  _ /= _ = False

