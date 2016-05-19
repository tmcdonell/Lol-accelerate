{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NoImplicitPrelude   #-}
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

) where

-- accelerate
import Data.Array.Accelerate                                        as A
import Data.Array.Accelerate.IO                                     as A

-- lol/lol-accelerate
import Crypto.Lol.Cyclotomic.Tensor.Accelerate.Common
import Crypto.Lol.LatticePrelude                                    as P
import qualified Crypto.Lol.Cyclotomic.Tensor                       as T

-- other libraries
import Control.Applicative                                          ( (<$>) )
import qualified Data.Vector.Unboxed                                as U


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
    => Tagged '(m,m') (Acc (Vector (Int,Bool)))
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

