{-|
Module      : BenchLolAccMain
Description : Main driver for lol benchmarks with AT.
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2017
License     : GPL-2
Maintainer  : ecrockett0@email.com
Stability   : experimental
Portability : POSIX

Main driver for lol benchmarks with AT.
-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE TypeOperators         #-}

module BenchLolAccMain where

import Crypto.Lol.Benchmarks
import Crypto.Lol.Cyclotomic.Tensor.Accelerate
import Crypto.Lol.Factored
import Crypto.Random.DRBG

import Data.Proxy
import Data.Array.Accelerate.Debug

-- choose which layers of Lol to benchmark
ls :: [String]
ls = [
  "STensor",
  "Tensor",
  "SUCyc",
  "UCyc",
  "Cyc"
  ]

-- choose which operations to benchmark
bs :: [String]
bs = [
  "unzipPow",
  "unzipDec",
  "unzipCRT",
  "zipWith (*)",
  "crt",
  "crtInv",
  "l",
  "lInv",
  "*g Pow",
  "*g Dec",
  "*g CRT",
  "divg Pow",
  "divg Dec",
  "divg CRT",
  "lift",
  "error",
  "twacePow",
  "twaceDec",
  "twaceCRT",
  "embedPow",
  "embedDec",
  "embedCRT"
  ]

main :: IO ()
main = do
  accInit
  beginMonitoring
  diagnosticMain

{-
tableMain :: IO ()
tableMain = do
  let opts = (T.defaultOpts $ Just "UCyc"){T.benches=bs}
  g1 <- defaultBenches (Proxy::Proxy AT)
  mapM_ (T.prettyBenches opts) g1
-}

diagnosticMain :: IO ()
diagnosticMain = do
  let opts = defaultDiagnosticOpts{levels=ls, benches=bs}
  b1 <- benchGroup "Single Index"
          [oneIdxBenches (Proxy::Proxy '(F64*F9*F25, Zq 14401)) (Proxy::Proxy AT) (Proxy::Proxy HashDRBG)]
  b2 <- benchGroup "Twace-Embed"
          [twoIdxBenches (Proxy::Proxy '(F64*F9*F25, F64*F9*F25, Zq 14401)) (Proxy::Proxy AT)]
  mapM_ (prettyBenchesDiagnostic opts) [b1,b2]

