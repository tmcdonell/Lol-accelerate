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

{-# LANGUAGE CPP #-}

module HomomPRFAccMain where

#ifdef WITH_APPS

import Crypto.Lol.Cyclotomic.Tensor.Accelerate
import Crypto.Lol.Applications.Examples.HomomPRF
import Data.Proxy

main :: IO ()
main = homomPRFMain (Proxy::Proxy AT)

#else

main :: IO ()
main = return ()

#endif