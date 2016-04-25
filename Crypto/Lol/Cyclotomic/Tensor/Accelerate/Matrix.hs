{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Crypto.Lol.Cyclotomic.Tensor.Accelerate.Matrix
-- Copyright   : [2016] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Kronecker products of matrices
--

module Crypto.Lol.Cyclotomic.Tensor.Accelerate.Matrix
  where

import Data.Array.Accelerate                                        ( Exp, DIM2, Z(..), (:.)(..) )
import qualified Data.Array.Accelerate                              as A

import qualified Data.Array.Accelerate.Algebra.Ring                 as Ring ()
import qualified Data.Array.Accelerate.Algebra.IntegralDomain       as IntegralDomain ()

import Crypto.Lol.LatticePrelude

import Data.Singletons.Prelude
import Control.Applicative


-- | A Kronecker product of zero or more matrices over @r@
--
data Matrix r
  = MNil
  | MKron (Matrix r) (MatrixC r)    -- snoc list

-- XXX: This type can be used to introduce nested parallelism and unconstrained
--      inlining... be careful.
--
data MatrixC r
  = MC DIM2 (Exp DIM2 -> Exp r)


-- | Extract the @(i,j) element of a 'Matrix'
--
indexM :: Ring (Exp r) => Matrix r -> Exp DIM2 -> Exp r
indexM MNil                          _  = one
indexM (MKron mat (MC (Z:.r:.c) mc)) ix =
  let
      Z :. i :. j = A.unlift ix
      (iq,ir)     = i `divMod` A.constant r
      (jq,jr)     = j `divMod` A.constant c
  in
  indexM mat (A.index2 iq jq) * mc (A.index2 ir jr)


fMatrix
    :: forall monad m r. (Fact m, Monad monad, Ring (Exp r))
    => (forall pp. PPow pp => TaggedT pp monad (MatrixC r))
    -> TaggedT m monad (Matrix r)
fMatrix mat = tagT $ go (sUnF (sing :: SFactored m))
  where
    go :: Sing (pplist :: [PrimePower]) -> monad (Matrix r)
    go SNil             = return MNil
    go (SCons spp rest) = MKron <$> go rest <*> withWitnessT mat spp


-- | For a prime power @p^e@, convert any matrix @M@ for prime @p@ to
-- @1_{p^{e-1}}@ ⊗ @M@, where @1@ denotes the all-1s vector.
--
ppMatrix
    :: forall monad pp r. (PPow pp, Monad monad, Ring (Exp r))
    => (forall p. Prim p => TaggedT p monad (MatrixC r))
    -> TaggedT pp monad (MatrixC r)
ppMatrix mat = tagT $ go (sing :: SPrimePower pp)
  where
    go :: SPrimePower pp -> monad (MatrixC r)
    go pp@(SPP (STuple2 sp _)) = do
      MC (Z:.h:.w) f <- withWitnessT mat sp
      let
          d = withWitness valuePPow  pp `div`
              withWitness valuePrime sp

          g :: Exp DIM2 -> Exp r
          g ix = let Z:.i:.j = A.unlift ix
                 in  f (A.index2 (i`mod`A.constant h) j)
      --
      return $ MC (Z:.h*d:.w) g

