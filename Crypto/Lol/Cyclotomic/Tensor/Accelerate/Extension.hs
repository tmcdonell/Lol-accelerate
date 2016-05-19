{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
-- |
-- Module      : Crypto.Lol.Cyclotomic.Tensor.Accelerate.Extension
-- Copyright   : [2016] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Functions for embedding and twacing in various bases
--

module Crypto.Lol.Cyclotomic.Tensor.Accelerate.Extension (

  twacePowDec,

) where

-- accelerate
import Data.Array.Accelerate                                        as A
import Data.Array.Accelerate.IO                                     as A
-- import Data.Array.Accelerate.Array.Data

-- lol/lol-accelerate
import Crypto.Lol.Cyclotomic.Tensor.Accelerate.Common

import Crypto.Lol.LatticePrelude                                    as P
import qualified Crypto.Lol.Cyclotomic.Tensor                       as T

-- other libraries
import qualified Data.Vector.Storable                               as S


-- | The "tweaked trace" function in either the powerful or decoding basis of
-- the m'`th cyclotomic ring to the m`th cyclotomic ring when @m | m'@.
--
twacePowDec
    :: forall m m' r. (m `Divides` m', Elt r)
    => Arr m' r
    -> Arr m  r
twacePowDec =
  let indices = proxy extIndicesPowDec (Proxy::Proxy '(m,m'))
  in  Arr . A.gather indices . unArr


-- Reindexing arrays, for convenience and speed.
--
-- TODO: Make sure these really are memoised.
--


extIndicesPowDec
    :: (m `Divides` m')
    => Tagged '(m,m') (Acc (Vector Int))
extIndicesPowDec = do
  idxs           <- T.extIndicesPowDec
  return . A.use $! intFromStorable idxs

-- | /O(1)/ Convert a Storable array of `Int` into an Accelerate array
--
intFromStorable :: S.Vector Int -> A.Vector Int
intFromStorable v = A.fromVectors (Z :. S.length v) v

