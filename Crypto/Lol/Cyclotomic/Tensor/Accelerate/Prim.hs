{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE NoImplicitPrelude       #-}
-- |
-- Module      : Crypto.Lol.Cyclotomic.Tensor.Accelerate.Prim
-- Copyright   : [2016] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Crypto.Lol.Cyclotomic.Tensor.Accelerate.Prim
  where

import Data.Array.Accelerate                                        as A
import qualified Data.Array.Accelerate.Algebra.ZeroTestable         as ZeroTestable
import qualified Data.Array.Accelerate.Algebra.IntegralDomain       as IntegralDomain ()

import Crypto.Lol.Prelude                                           hiding ( ZeroTestable )

type ZeroTestable a = ZeroTestable.C a


-- Implementation of the core operations. The intention is that instances of
-- this class are 'runQ' ahead-of-time compiled, rather than 'runN' online
-- compiled.
--
class Elt r => Dispatch r where

  -- misc
  expose'       :: Scalar Int -> Scalar Int -> Vector r -> Array DIM2 r
  unexpose'     :: Scalar Int -> Array DIM2 r -> Vector r
  eq            :: A.Eq r => Vector r -> Vector r -> Scalar Bool
  neq           :: A.Eq r => Vector r -> Vector r -> Scalar Bool

  -- operations from GL
  pL'           :: Additive (Exp r) => Array DIM2 r -> Array DIM2 r
  pLInv'        :: Additive (Exp r) => Array DIM2 r -> Array DIM2 r
  pGPow'        :: Additive (Exp r) => Scalar Int -> Array DIM2 r -> Array DIM2 r
  pGDec'        :: Additive (Exp r) => Array DIM2 r -> Array DIM2 r
  pGInvPow'     :: (Ring (Exp r), A.FromIntegral Int r) => Scalar Int -> Array DIM2 r -> Array DIM2 r
  pGInvDec'     :: (Ring (Exp r), A.FromIntegral Int r) => Scalar Int -> Array DIM2 r -> Array DIM2 r
  divCheck'     :: (IntegralDomain (Exp r), ZeroTestable (Exp r), Elt r) => Vector r -> Scalar r -> (Scalar Bool, Vector r)

  pGInvPow' = error "only applicable for IntegralDomain types"
  pGInvDec' = error "only applicable for IntegralDomain types"
  divCheck' = error "only applicable for IntegralDomain types"

  -- operations from Pow
  embedPow'     :: Additive (Exp r) => Vector (Int,Int) -> Vector r -> Vector r

  -- operations from CRT
  embedCRT'     :: Vector Int -> Vector r -> Vector r

  -- operations from Extension
  -- twacePowDec'  :: Vector Int -> Vector r -> Vector r

