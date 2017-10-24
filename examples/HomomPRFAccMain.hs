{-|
Module      : HomomPRFAccMain
Description : Example, test, and macro-benchmark for homomorphic evaluation of a PRF with AT.
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2017
License     : GPL-2
Maintainer  : ecrockett0@email.com
Stability   : experimental
Portability : POSIX

Example, test, and macro-benchmark for homomorphic evaluation of a PRF with AT.
-}

module HomomPRFAccMain where

import Crypto.Lol.Applications.Examples
import Crypto.Lol.Cyclotomic.Tensor.Accelerate
import Data.Array.Accelerate.Debug
import Data.Proxy

main :: IO ()
main = do
  accInit
  beginMonitoring
  homomPRFMain (Proxy::Proxy AT)

