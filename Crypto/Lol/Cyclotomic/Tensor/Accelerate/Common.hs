{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RoleAnnotations     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
-- |
-- Module      : Crypto.Lol.Cyclotomic.Tensor.Accelerate.Common
-- Copyright   : [2016] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Crypto.Lol.Cyclotomic.Tensor.Accelerate.Common (

  Arr(..),
  repl, eval, evalM,
  fTensor, ppTensor,
  Trans(Id), trans, dim, (.*), (@*),
  mulMat, mulDiag,

  -- utilities
  wrap, wrap2, wrapM,
  ($$),

) where

import Data.Array.Accelerate                                        ( Acc, Array, Scalar, Vector, Exp, Elt, DIM2, All(..), Z(..), (:.)(..), (!) )
import qualified Data.Array.Accelerate                              as A

import qualified Data.Array.Accelerate.Algebra.Additive             as Additive ()
import qualified Data.Array.Accelerate.Algebra.IntegralDomain       as IntegralDomain ()
import qualified Data.Array.Accelerate.Algebra.Ring                 as Ring ()

import Crypto.Lol.Cyclotomic.Tensor.Accelerate.Backend
import Crypto.Lol.Cyclotomic.Tensor.Accelerate.Memo
import Crypto.Lol.Prelude

import Data.Singletons.Prelude                                      ( Sing(..), sing )

import Control.Applicative
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Random
import Data.HashMap.Strict                                          ( HashMap )
import Data.Typeable
import System.IO.Unsafe
import Text.Printf
import qualified Data.HashMap.Strict                                as Map


-- infixr 9 .*
-- infixl 7 @*

-- | Indexed newtype representing arrays that hold elements of some type 'r'. In
-- this case, backed by Accelerate.
--
-- XXX: Why do we store this as a regular 1D array? Need to properly understand
--      what 'expose' and 'unexpose' are doing.
--
data Arr (m :: Factored) r = Arr { unArr :: !(Vector r) }
  deriving Show

-- Although the first argument is phantom, it affects the representation.
--
-- <https://ghc.haskell.org/trac/ghc/wiki/Roles#Thesolution>
--
type role Arr nominal nominal

-- This forces the arguments to be evaluated, so it might be good store this
-- result and under-the-hood update the operands to now be a 'use', rather than
-- some (possibly quite large) expression.
--
instance A.Eq r => Eq (Arr m r) where
  Arr xs == Arr ys = A.indexArray (go xs ys) Z
    where
      go = memo' __eq (typeRep (Proxy::Proxy r))
         $ runN (A.and $$ A.zipWith (A.==))
  --
  Arr xs /= Arr ys = A.indexArray (go xs ys) Z
    where
      go = memo' __neq (typeRep (Proxy::Proxy r))
         $ runN (A.or  $$ A.zipWith (A./=))

instance (Elt r, Random r, Fact m) => Random (Arr m r) where
  randomR = error "Arr.randomR: not supported"
  random  =
    let n  = proxy totientFact (Proxy::Proxy m)
        sh = Z :. n
    in
    runRand $ do
      xs     <- replicateM n (liftRand random)
      return $! Arr (A.fromList sh xs)


{-# INLINE wrap #-}
wrap :: (Elt a, Elt b)
     => (Vector a -> Vector b)
     -> Arr ma a
     -> Arr mb b
wrap f = Arr . f . unArr

{-# INLINE wrap2 #-}
wrap2 :: (Elt a, Elt b, Elt c)
      => (Vector a -> Vector b -> Vector c)
      -> Arr ma a
      -> Arr mb b
      -> Arr mc c
wrap2 f (Arr xs) (Arr ys) = Arr $ f xs ys

{-# INLINE wrapM #-}
wrapM :: (Monad monad, Elt a, Elt b)
      => (Vector a -> monad (Vector b))
      -> Arr ma a
      -> monad (Arr mb b)
wrapM f (Arr xs) = Arr <$> f xs


-- | 'Tensorable represents an "atomic" transform over the base type 'r' that
-- can be augmented (tensored) on the left and right by identity transforms of
-- any dimension. It has the following components:
--
--  * The dimension 'd' of the atomic transform 'f'
--
--  * A function 'f' that given any dimensions 'l', 'r', applies the
--    'ldr'-dimensional transform 'I_l' ⊗ 'f' ⊗ 'I_r' to an array of
--    elements of type 'r'.
--
type Tensorable r = ( Int, Acc (Array DIM2 r) -> Acc (Array DIM2 r) )

-- | A transform with particular 'I_l', 'I_r'
--
type TransC r = ( Tensorable r, Int, Int )

-- | A full transform as a sequence of zero or more components terminated by the
-- identity transform. To be well-formed, all components must have the same
-- dimensionality.
--
data Trans r where
  Id    :: Int                 -> Trans r   -- identity sentinel
  TSnoc :: Trans r -> TransC r -> Trans r   -- transform function composition


-- | An 'Arr' filled with the argument.
--
repl :: forall m r. (Fact m, Elt r) => Exp r -> Arr m r
repl r =
  let n = proxy totientFact (Proxy :: Proxy m)
  in  Arr . run $ A.fill (A.constant (Z :. n)) r


-- | Smart constructor for 'Trans'
--
trans :: Tensorable r -> Trans r
trans f@(d,_) = Id d `TSnoc` (f, 1, 1)   -- I_d . f


-- | Kronecker-product operator ⊗
--
(@*) :: Trans r -> Trans r -> Trans r
Id m               @* Id n               = Id (m * n)                             -- Merge identity transforms: I_m ⊗ I_n = I_mn
i@(Id n)           @* TSnoc g' (g, l, r) = TSnoc (i @* g') (g, n*l, r)            -- Id on left: I_n ⊗ (A ○ B) = (I_n ⊗ A) ○ (I_n ⊗ B)
TSnoc f' (f, l, r) @* i@(Id n)           = TSnoc (f' @* i) (f, l, r*n)            -- ...and similarly to above with Id on the right
f                  @* g                  = (f @* Id (dim g)) .* (Id (dim f) @* g) -- compose: (A ⊗ B) = (A ⊗ I) ○ (I ⊗ B)


-- | Composition of transforms; corresponds to matrix multiplication.
--
(.*) :: Trans r -> Trans r -> Trans r
f .* g
  | dim f == dim g = f ..* g
  | otherwise      = error $ printf "(.*): transform dimensions mismatch: %d /= %d" (dim f) (dim g)
  where
    f' ..* (Id _)          = f'                     -- drop sentinel
    f' ..* (TSnoc rest g') = TSnoc (f' ..* rest) g'


-- | Returns the (linear) dimension of a transform
--
dim :: Trans r -> Int
dim (Id n)      = n
dim (TSnoc _ f) = dimC f        -- just use dimension of head

dimC :: TransC r -> Int
dimC ((d, _), l, r) = l*d*r


-- | Evaluate a transform by evaluating each component in sequence
--
eval :: Elt r
     => Tagged m (Trans r)
     -> Tagged m (Acc (Vector r) -> Acc (Vector r))
eval t = tag (go (untag t))
  where
    go Id{}           = id
    go (TSnoc rest f) = go rest . evalC f

evalC :: Elt r => TransC r -> Acc (Vector r) -> Acc (Vector r)
evalC ((d, f), _, r) = unexpose r . f . expose d r

evalM :: (Elt r, Monad monad)
      => TaggedT m monad (Trans r)
      -> monad (Tagged m (Acc (Vector r) -> Acc (Vector r)))
evalM = fmap (eval . return) . untagT


-- | Map the innermost dimension to a 2D array with innermost dimension 'd' for
-- performing 'I_l' ⊗ 'I_r' transformation.
--
expose :: Elt r => Int -> Int -> Acc (Vector r) -> Acc (Array DIM2 r)
expose d r arr = res
  where
    res | r == 1    = A.reshape sh arr          -- NOP if arr is manifest
        | otherwise = A.backpermute sh f arr
    --
    d'      = A.constant d
    r'      = A.constant r
    sz      = A.unindex1 (A.shape arr)
    sh      = A.index2 (sz `div` d') d'
    f ix    = let Z :. i :. j = A.unlift ix
                  imodr       = i `mod` r'
              in
              A.index1 ((i-imodr)*d' + j*r' + imodr)

-- | Inverse of 'expose'
--
unexpose :: Elt r => Int -> Acc (Array DIM2 r) -> Acc (Vector r)
unexpose 1 arr = A.flatten arr                  -- NOP if arr is manifest
unexpose r arr = A.backpermute sh f arr
  where
    sh            = A.index1 (sz * d)
    Z :. sz :. d  = A.unlift (A.shape arr)
    f ix          = let i              = A.unindex1 ix
                        (idivr, imodr) = i     `divMod` A.constant r
                        (idivrd, j)    = idivr `divMod` d
                    in
                    A.index2 (A.constant r * idivrd + imodr) j

-- | For a factored index, tensors up any function defined for (and tagged by)
-- any prime power
--
fTensor
    :: forall m r monad. (Fact m, Monad monad)
    => (forall pp. PPow pp => TaggedT pp monad (Trans r))
    -> TaggedT m monad (Trans r)
fTensor f = tagT . go $ sUnF (sing :: SFactored m)
  where
    go :: Sing (pplist :: [PrimePower]) -> monad (Trans r)
    go SNil         = return (Id 1)
    go (SCons s ss) = do
      ss' <- go ss
      f'  <- withWitnessT f s
      return $ ss' @* f'

-- | For a prime power @p^e@, tensors up any function @f@ defined for (and
-- tagged by) a prime to @I_{p^{e-1}} ⊗ f@
--
ppTensor
    :: forall pp r monad. (PPow pp, Monad monad)
    => (forall p. Prime p => TaggedT p monad (Trans r))
    -> TaggedT pp monad (Trans r)
ppTensor f = tagT $ go (sing :: SPrimePower pp)
  where
    go pp@(SPP (STuple2 sp _)) = do
      f' <- withWitnessT f sp
      let x   = withWitness valuePPow pp
          y   = withWitness valuePrime sp
          lts = x `div` y
      return $ Id lts @* f'


-- | General matrix-matrix multiplication
--
-- TODO: Use the FFI to import a fast implementation. Note that this
-- implementation is different to the standard row-major implementation of
-- matrix-multiply we usually use.
--
mulMat
    :: (Ring (Exp r), Elt r)
    => Acc (Array DIM2 r)
    -> Acc (Array DIM2 r)
    -> Acc (Array DIM2 r)
mulMat arr brr
  = A.fold (+) zero
  $ A.zipWith (*) arr' brr'
  where
    Z :. _     :. rowsA = A.unlift (A.shape arr) :: Z :. Exp Int :. Exp Int
    Z :. colsB :. _     = A.unlift (A.shape brr) :: Z :. Exp Int :. Exp Int
    arr'                = A.replicate (A.lift (Z :. colsB :. All   :. All)) arr
    brr'                = A.replicate (A.lift (Z :. All   :. rowsA :. All)) brr


-- | Multiplication by a diagonal matrix along the innermost dimension
--
mulDiag
    :: (Ring (Exp r), Elt r)
    => Acc (Vector r)
    -> Acc (Array DIM2 r)
    -> Acc (Array DIM2 r)
mulDiag diag mat
  = A.generate (A.shape mat)
  $ \ix -> mat  ! ix
         * diag ! A.index1 (A.indexHead ix)


-- Auxiliary functions
-- --------------------

infixr 0 $$
($$) :: (b -> a) -> (c -> d -> b) -> c -> d -> a
(f $$ g) x y = f (g x y)


-- Memo tables
-- -----------

__eq, __neq :: MVar (HashMap TypeRep (Vector r -> Vector r -> Scalar Bool))
__eq  = unsafePerformIO $ newMVar Map.empty
__neq = unsafePerformIO $ newMVar Map.empty

