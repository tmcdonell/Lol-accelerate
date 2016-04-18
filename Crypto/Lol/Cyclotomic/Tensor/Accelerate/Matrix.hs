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

import Data.Array.Accelerate                                        as A

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

-- TODO: Use DIM2 to make the dimensionality & row/column indexes clear?
--
data MatrixC r
  = MC Int Int                      -- #rows, #columns
    (Exp Int -> Exp Int -> r)       -- indexing with 'Exp Int'


-- | Extract the @(i,j) element of a 'Matrix'
--
indexM :: Ring (Exp r) => Matrix (Exp r) -> Exp Int -> Exp Int -> Exp r
indexM MNil                    _ _ = one
indexM (MKron mat (MC r c mc)) i j =
  let
      (iq,ir) = i `divMod` constant r
      (jq,jr) = j `divMod` constant c
  in
  indexM mat iq jq * mc ir jr


fMatrix
    :: forall monad m r. (Fact m, Monad monad, Ring (Exp r))
    => (forall pp. PPow pp => TaggedT pp monad (MatrixC (Exp r)))
    -> TaggedT m monad (Matrix (Exp r))
fMatrix mat = tagT $ go (sUnF (sing :: SFactored m))
  where
    go :: Sing (pplist :: [PrimePower]) -> monad (Matrix (Exp r))
    go SNil             = return MNil
    go (SCons spp rest) = MKron <$> go rest <*> withWitnessT mat spp


-- | For a prime power @p^e@, convert any matrix @M@ for prime @p@ to
-- @1_{p^{e-1}}@ ⊗ @M@, where @1@ denotes the all-1s vector.
--
ppMatrix
    :: forall monad pp r. (PPow pp, Monad monad, Ring (Exp r))
    => (forall p. Prim p => TaggedT p monad (MatrixC (Exp r)))
    -> TaggedT pp monad (MatrixC (Exp r))
ppMatrix mat = tagT $ go (sing :: SPrimePower pp)
  where
    go :: SPrimePower pp -> monad (MatrixC (Exp r))
    go pp@(SPP (STuple2 sp _)) = do
      MC h w f <- withWitnessT mat sp
      let
          d = withWitness valuePPow  pp `div`
              withWitness valuePrime sp
      --
      return $ MC (h*d) w (f . (`mod` constant h))

