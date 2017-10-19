{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module      : Crypto.Lol.Cyclotomic.Tensor.Accelerate.TH
-- Copyright   : [2016] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Crypto.Lol.Cyclotomic.Tensor.Accelerate.TH
  where

import Data.Proxy
import Crypto.Lol.Prelude                                           hiding ( ZeroTestable )

import Data.Array.Accelerate                                        as A
import qualified Data.Array.Accelerate.Algebra.ZeroTestable         as ZeroTestable
import qualified Data.Array.Accelerate.Algebra.IntegralDomain       as IntegralDomain ()

import Crypto.Lol.Cyclotomic.Tensor.Accelerate.Common
import Crypto.Lol.Cyclotomic.Tensor.Accelerate.GL
import Crypto.Lol.Cyclotomic.Tensor.Accelerate.Pow

type ZeroTestable a = ZeroTestable.C a


-- Common
-- ------

_expose :: Elt r => Proxy r -> Acc (Scalar Int) -> Acc (Scalar Int) -> Acc (Vector r) -> Acc (Array DIM2 r)
_expose _ = expose'

_unexpose :: Elt r => Proxy r -> Acc (Scalar Int) -> Acc (Array DIM2 r) -> Acc (Vector r)
_unexpose _ = unexpose'

_eq :: A.Eq r => Proxy r -> Acc (Vector r) -> Acc (Vector r) -> Acc (Scalar Bool)
_eq _ xs ys = A.and (A.zipWith (A.==) xs ys)

_neq :: A.Eq r => Proxy r -> Acc (Vector r) -> Acc (Vector r) -> Acc (Scalar Bool)
_neq _ xs ys = A.or (A.zipWith (A./=) xs ys)


-- GL
-- --

_pL :: (Additive (Exp r), Elt r) => Proxy r -> Acc (Array DIM2 r) -> Acc (Array DIM2 r)
_pL _ = pL'

_pLInv :: (Additive (Exp r), Elt r) => Proxy r -> Acc (Array DIM2 r) -> Acc (Array DIM2 r)
_pLInv _ = pLInv'

_pGPow :: (Additive (Exp r), Elt r) => Proxy r -> Acc (Scalar Int) -> Acc (Array DIM2 r) -> Acc (Array DIM2 r)
_pGPow _ = pGPow'

_pGDec :: (Additive (Exp r), Elt r) => Proxy r -> Acc (Array DIM2 r) -> Acc (Array DIM2 r)
_pGDec _ = pGDec'

_pGInvPow :: (Ring (Exp r), A.FromIntegral Int r, Elt r) => Proxy r -> Acc (Scalar Int) -> Acc (Array DIM2 r) -> Acc (Array DIM2 r)
_pGInvPow _ = pGInvPow'

_pGInvDec :: (Ring (Exp r), A.FromIntegral Int r, Elt r) => Proxy r -> Acc (Scalar Int) -> Acc (Array DIM2 r) -> Acc (Array DIM2 r)
_pGInvDec _ = pGInvDec'

_divCheck :: (IntegralDomain (Exp r), ZeroTestable (Exp r), Elt r) => Proxy r -> Acc (Vector r) -> Acc (Scalar r) -> Acc (Scalar Bool, Vector r)
_divCheck _ = divCheck'


-- Pow
-- ---

_embedPow :: (Additive (Exp r), Elt r) => Proxy r -> Acc (Vector (Int,Int)) -> Acc (Vector r) -> Acc (Vector r)
_embedPow _ = embedPow'

