{-|
Module      : SHEAccMain
Description : Example using SymmSHE with AT.
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2017
License     : GPL-2
Maintainer  : ecrockett0@email.com
Stability   : experimental
Portability : POSIX

Example using SymmSHE with AT.
-}

module SHEAccMain where

import Crypto.Lol.Applications.Examples
import Crypto.Lol.Cyclotomic.Tensor.Accelerate
import Data.Array.Accelerate.Debug
import Data.Proxy

main :: IO ()
main = do
  accInit
  beginMonitoring
  sheMain (Proxy::Proxy AT)

