{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE RebindableSyntax    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
-- |
-- Module      : Crypto.Lol.Cyclotomic.Tensor.Accelerate.Dec
-- Copyright   : [2016] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Linear transforms and operations relating to the Decoding basis in
-- Accelerate.
--

module Crypto.Lol.Cyclotomic.Tensor.Accelerate.Dec (

  embed,
  tGaussian, gSqNorm,

) where

-- accelerate
import Data.Array.Accelerate                                        ( Acc, Array, DIM1, Exp, Elt, FromIntegral, Z(..), (:.)(..), All(..), (<*), (?) )
import Data.Array.Accelerate.IO                                     as A
import qualified Data.Array.Accelerate                              as A

-- lol/lol-accelerate
import Crypto.Lol.Cyclotomic.Tensor.Accelerate.Backend
import Crypto.Lol.Cyclotomic.Tensor.Accelerate.Common

import Crypto.Lol.GaussRandom                                       ( realGaussian )
import Crypto.Lol.Prelude                                    as P hiding (FromIntegral)
import qualified Crypto.Lol.Cyclotomic.Tensor                       as T

-- other libraries
import Data.Word
import Control.Applicative                                          ( (<$>) )
import Control.Monad
import Control.Monad.Random
import qualified Data.Vector.Unboxed                                as U
-- import qualified Data.Vector.Storable                               as S


-- | Embeds an array in the decoding basis of the m`th cyclotomic ring to an
-- array in the decoding basis of the m'`th cyclotomic ring, when @m | m'@.
--
embed :: forall m m' r. (m `Divides` m', Additive (Exp r), Elt r)
      => Arr m  r
      -> Arr m' r
embed (Arr arr) =
  let
      indices = proxy baseIndicesDec (Proxy::Proxy '(m,m'))
      f ib    =
        let (i,b) = A.unlift ib
            x     = arr A.!! i            -- XXX: check this is not lifted out of (?)
        in i <* zero ? ( zero             -- [See note: baseIndicesDec]
         , b         ? ( P.negate x, x ))
  in
  Arr $ A.map f indices


-- | Given @v=r^2@, yields the decoding-basis coefficients of a sample from the
-- tweaked Gaussian @t_m \cdot D_r@
--
-- Note that the random numbers are generated sequentially on the host.
--
tGaussian
    :: forall m v r random. ( Fact m, MonadRandom random, ToRational v, Ord r, Random r, Transcendental r
                            , Transcendental (Exp r), FromIntegral Int r, Elt r)
    => v
    -> random (Arr m r)
tGaussian v = do
  let
      m   = proxy valueFact   (Proxy::Proxy m)
      n   = proxy totientFact (Proxy::Proxy m)
      rad = proxy radicalFact (Proxy::Proxy m)
  --
  x <- A.fromList (Z :. n) <$> realGaussians (v * fromIntegral (m `div` rad)) n
  return $ fE (Arr (A.use x))


-- | The @E_m@ transformation for an arbitrary @m@.
--
fE :: (Fact m, Transcendental (Exp r), FromIntegral Int r, Elt r)
   => Arr m r
   -> Arr m r
fE = eval $ fTensor $ ppTensor pE

-- | The @E_p@ transformation for a prime @p@.
--
pE :: forall p r. (Prime p, Transcendental (Exp r), A.FromIntegral Int r, Elt r)
   => Tagged p (Trans r)
pE =
  let
      pval = proxy valuePrime (Proxy::Proxy p)
      mat  = A.generate (A.constant (Z :. pval-1 :. pval -1))
           $ \ix -> let Z :. i :. j = A.unlift ix :: Z :. Exp Int :. Exp Int
                        theta       = 2 * pi * A.fromIntegral (i*(j+1)) / P.fromIntegral pval
                        q           = j <* A.constant (pval `div` 2)
                                    ? ( cos theta, sin theta )
                    in
                    sqrt 2 * q
  in
  tag $ if pval == 2
           then Id 2
           else trans (pval-1, mulMat mat)


-- | Generate @n@ real, independent gaussians of scaled variance
-- @svar = true variance @ (2*pi)@
--
realGaussians
    :: (ToRational svar, Ord r, Transcendental r, Random r, MonadRandom random)
    => svar
    -> Int
    -> random [r]
realGaussians var n
  | odd n     = P.tail <$> realGaussians var (n+1)
  | otherwise = uncurry (++) . P.unzip <$> replicateM (n `div` 2) (realGaussian var)


-- | Given coefficient tensor @e@ with respect to the decoding basis of @R@,
-- yield the (scaled) squared norm of @g_m \cdot e@ under the canonical
-- embedding, namely:
--
--   @\hat{m}^{-1} \cdot || \sigma ( g_m - \cdot e) || ^ 2@
--
gSqNorm :: (Fact m, Ring (Exp r), Elt r) => Arr m r -> r
gSqNorm e =
  let e' = fGram e
      r  = A.foldAll (+) zero
         $ A.zipWith (*) (unArr e) (unArr e')
  in
  run r `A.indexArray` Z


-- | Multiply by @\hat{m}@ times the Gram matrix of decoding basis @R^vee@
--
fGram :: (Fact m, Ring (Exp r), Elt r) => Arr m r -> Arr m r
fGram = eval $ fTensor $ ppTensor pGramDec

-- | Multiply by the (scaled) Gram matrix of decoding basis: @I_{p-1} + all-1s@
--
pGramDec
    :: forall p r. (Prime p, Ring (Exp r), Elt r)
    => Tagged p (Trans r)
pGramDec =
  let
      pval  = proxy valuePrime (Proxy::Proxy p)
      f arr =
        let
            ss  = A.fold (+) zero arr
            ss' = A.replicate (A.lift (Z :. All :. w)) ss
            w   = A.indexHead (A.shape arr)
        in
        A.zipWith (+) arr ss'
  in
  tag $ if pval == 2
           then Id 1
           else trans (pval-1, f)


-- Reindexing arrays
-- -----------------
--
-- TODO: Make sure these really are memoised.
--

-- NOTE:
--
--  * Since we don't have sum types in Accelerate, we encode the first element
--    of the tuple (the new index) to (-1) on Nothing, otherwise return the
--    tuple elements unchanged.
--  * unzip of the unboxed vector is O(1)
--  * conversion to storable vectors is O(n)
--  * conversion from storable to Accelerate is O(1)
--
baseIndicesDec
    :: forall m m'. (m `Divides` m')
    => Tagged '(m,m') (Acc (Array DIM1 (Int,Bool)))
baseIndicesDec = do
  (ix,b) <- U.unzip
          . U.map (maybe (-1,0) (\(i,b) -> (i, fromBool b)))
        <$> T.baseIndicesDec
  --
  let ix' = U.convert ix
      b'  = U.convert b
  return . A.use $! A.fromVectors (Z :. U.length ix) (((), ix'), b')

-- To match the encoding of Booleans in Accelerate
--
{-# INLINE fromBool #-}
fromBool :: Bool -> Word8
fromBool True  = 1
fromBool False = 0

