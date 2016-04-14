{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RoleAnnotations     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}
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
  repl, eval,
  fTensor, ppTensor,
  Trans(Id), trans, dim, (.*), (@*),

) where

import Data.Array.Accelerate                                        as A

import qualified Data.Array.Accelerate.Algebra.Ring                 as Ring ()
import qualified Data.Array.Accelerate.Algebra.Additive             as Additive ()
import qualified Data.Array.Accelerate.Algebra.IntegralDomain       as IntegralDomain ()

import Crypto.Lol.LatticePrelude

import Data.Singletons
import Data.Singletons.Prelude

import Text.Printf


-- | Indexed newtype representing arrays that hold elements of some type 'r'. In
-- this case, backed by Accelerate.
--
newtype Arr (m :: Factored) r = Arr { unArr :: Acc (Array DIM1 r) }
  deriving Show

-- Although the first argument is phantom, it affects the representation.
--
-- <https://ghc.haskell.org/trac/ghc/wiki/Roles#Thesolution>
--
type role Arr nominal nominal


-- | 'Tensorable represents a "atomic" transform over the base type 'r'. that
-- can be augmented (tensored) on the lift and right by identity transforms of
-- any dimension. It has the following components:
--
--  * The dimension 'd' of the atomic transform 'f'
--
--  * A function 'f' that given any dimensions 'l', 'r', applies the
--    'ldr'-dimensional transform 'I_l' &#8855; f &#8855; 'I_r' to an array of
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
  in  Arr $ A.fill (constant (Z :. n)) r


-- | Smart constructor for 'Trans'
--
trans :: Tensorable r -> Trans r
trans f@(d,_) = Id d `TSnoc` (f, 1, 1)   -- I_d . f


-- | Kronecker-product operator &#8855;
--
(@*) :: Trans r -> Trans r -> Trans r
Id m @* Id n                   = Id (m * n)                               -- Merge identity transforms: I_m ⊗ I_n = I_mn
i@(Id n) @* TSnoc g' (g, l, r) = TSnoc (i @* g') (g, n*l, r)              -- Id on left or right: I_n ⊗ (A ○ B) = (I_n ⊗ A) ○ (I_n ⊗ B)
TSnoc f' (f, l, r) @* i@(Id n) = TSnoc (f' @* i) (f, l, r*n)              -- ...and similarly to above
f @* g                         = (f @* Id (dim g)) .* (Id (dim f) @* g)   -- compose: (A ⊗ B) = (A ⊗ I) ○ (I ⊗ B)


-- | ???
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
eval :: Elt r => Tagged m (Trans r) -> Arr m r -> Arr m r
eval = eval' . untag
  where
    eval' Id{}           = id
    eval' (TSnoc rest f) = eval' rest . evalC f

evalC :: Elt r => TransC r -> Arr m r -> Arr m r
evalC ((d, f), _, r) = Arr . unexpose r . f . expose d r . unArr


-- | Map the innermost dimension to a 2D array with innermost dimension 'd' for
-- performing 'I_l' &#8855; 'I_r' transformation.
--
expose :: Elt r => Int -> Int -> Acc (Array DIM1 r) -> Acc (Array DIM2 r)
expose (constant -> d) (constant -> r) arr = backpermute sh f arr
  where
    Z :. sz = unlift (shape arr)
    sh      = index2 (sz `div` d) d
    f ix    = let Z :. i :. j = unlift ix
                  imodr       = i `mod` r
              in
              index1 ((i-imodr)*d + j*r + imodr)

-- | Inverse of 'expose'
--
unexpose :: Elt r => Int -> Acc (Array DIM2 r) -> Acc (Array DIM1 r)
unexpose (constant -> r) arr = backpermute sh f arr
  where
    sh            = index1 (sz * d)
    Z :. sz :. d  = unlift (shape arr)
    f ix          = let i              = unindex1 ix
                        (idivr, imodr) = i     `divMod` r
                        (idivrd, j)    = idivr `divMod` d
                    in
                    index2 (r * idivrd + imodr) j

-- | For a factored index, tensors up any function defined for (and tagged by)
-- any prime power
--
fTensor
    :: forall m r monad. (Fact m, Monad monad, Elt r)
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
-- tagged by) a prime to @I_(p^{e-1}) &#8855; f@
--
ppTensor
    :: forall pp r monad. (PPow pp, Monad monad)
    => (forall p. Prim p => TaggedT p monad (Trans r))
    -> TaggedT pp monad (Trans r)
ppTensor f = tagT $ go (sing :: SPrimePower pp)
  where
    go pp@(SPP (STuple2 sp _)) = do
      f' <- withWitnessT f sp
      let x   = withWitness valuePPow pp
          y   = withWitness valuePrime sp
          lts = x `div` y
      return $ Id lts @* f'

