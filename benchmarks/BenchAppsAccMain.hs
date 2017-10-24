{-|
Module      : BenchAppsAccMain
Description : Main driver for lol-apps benchmarks with AT.
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2017
License     : GPL-2
Maintainer  : ecrockett0@email.com
Stability   : experimental
Portability : POSIX

Main driver for lol-apps benchmarks with AT.
-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}

module BenchAppsAccMain where

import Crypto.Lol
import Crypto.Lol.Applications.Benchmarks
import Crypto.Lol.Benchmarks
import Crypto.Lol.Cyclotomic.Tensor.Accelerate
import Crypto.Random.DRBG

import Data.Array.Accelerate.Debug

main :: IO ()
main = do
  let o   = (defaultTableOpts Nothing)
      pct = Proxy::Proxy AT
  --
  accInit
  beginMonitoring
  bs <- sequence $
          defaultSHEBenches pct (Proxy::Proxy TrivGad) (Proxy::Proxy HashDRBG) ++
          [defaultKHPRFBenches pct (Proxy::Proxy (BaseBGad 2))]
  mapM_ (prettyBenchesTable o) bs

