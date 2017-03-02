{-|
Module      : TestLolAccMain
Description : Main driver for lol tests with AT.
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2017
License     : GPL-2
Maintainer  : ecrockett0@email.com
Stability   : experimental
Portability : POSIX

Main driver for lol tests with AT.
-}

module TestLolAccMain where

import Crypto.Lol.Cyclotomic.Tensor.Accelerate
import Crypto.Lol.Tests
import Data.Proxy
import Test.Framework

main :: IO ()
main = defaultMainWithArgs
  (defaultLolTests (Proxy::Proxy AT)) ["--maximum-generated-tests=5"]