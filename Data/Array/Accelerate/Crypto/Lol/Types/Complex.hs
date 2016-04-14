{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.Crypto.Lol.Types.Complex
-- Copyright   : [2016] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Crypto.Lol.Types.Complex
  where

import Data.Array.Accelerate
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Product
import Data.Array.Accelerate.Array.Sugar

import Crypto.Lol.Types.Complex
import qualified Number.Complex                             as C


type instance EltRepr (Complex a) = EltRepr (a, a)

instance Elt a => Elt (Complex a) where
  eltType _             = eltType (undefined :: (a,a))
  toElt p               = let (a, b) = toElt p in Complex (a C.+: b)
  fromElt (Complex c)   = fromElt (C.real c, C.imag c)

instance cst a => IsProduct cst (Complex a) where
  type ProdRepr (Complex a) = ProdRepr (a, a)
  fromProd cst (Complex c) = fromProd cst (C.real c, C.imag c)
  toProd cst p          = let (x, y) = toProd cst p in Complex (x C.+: y)
  prod cst _            = prod cst (undefined :: (a, a))

instance (Lift Exp a, Elt (Plain a)) => Lift Exp (Complex a) where
  type Plain (Complex a) = Complex (Plain a)
  lift (Complex c)       = Exp $ Tuple (NilTup `SnocTup` lift (C.real c) `SnocTup` lift (C.imag c))

instance Elt a => Unlift Exp (Complex (Exp a)) where
  unlift e
    = let x     = Exp $ SuccTupIdx ZeroTupIdx `Prj` e
          y     = Exp $ ZeroTupIdx `Prj` e
      in
      Complex (x C.+: y)

