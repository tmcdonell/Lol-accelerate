{-|
Module      : KHPRFAccMain
Description : Example using KeyHomomorphicPRF with AT.
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2017
License     : GPL-2
Maintainer  : ecrockett0@email.com
Stability   : experimental
Portability : POSIX

Example using KeyHomomorphicPRF with AT.
-}

module KHPRFAccMain where

import Crypto.Lol.Applications.Examples
import Crypto.Lol.Cyclotomic.Tensor.Accelerate
import Data.Array.Accelerate.Debug
import Data.Proxy

main :: IO ()
main = do
  accInit
  khprfRingMain (Proxy::Proxy AT)

