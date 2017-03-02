{-|
Module      : TestAppsAccMain
Description : Main driver for lol-apps tests with AT.
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2017
License     : GPL-2
Maintainer  : ecrockett0@email.com
Stability   : experimental
Portability : POSIX

Main driver for lol-apps tests with AT.
-}

module TestAppsAccMain where

import Crypto.Lol (TrivGad)
import Crypto.Lol.Cyclotomic.Tensor.Accelerate
import Crypto.Lol.Applications.Tests
import Data.Proxy

import System.Environment
import Test.Framework

main :: IO ()
main = do
  opts <- testOptions
  flip defaultMainWithOpts opts $
    defaultAppsTests (Proxy::Proxy AT) (Proxy::Proxy TrivGad)

testOptions :: IO RunnerOptions
testOptions = do
  argv <- getArgs
  let -- Filter out Accelerate debugging flags, otherwise test-framework complains
      (before, r1)  = span (/= "+ACC") argv
      (_,      r2)  = span (/= "-ACC") $ dropWhile (== "+ACC") r1
      after         = dropWhile (== "-ACC") r2
  --
  mopts <- interpretArgs $ "--threads=1" : "--maximum-generated-tests=5" : before ++ after
  case mopts of
    Left err       -> error err
    Right (opts,_) -> return opts
