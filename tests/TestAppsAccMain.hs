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

import Data.Array.Accelerate.Debug
import Data.Proxy
import System.Environment
import Test.Framework

main :: IO ()
main = do
  accInit
  opts <- testOptions
  flip defaultMainWithOpts opts $
    defaultAppsTests (Proxy::Proxy AT) (Proxy::Proxy TrivGad)

testOptions :: IO RunnerOptions
testOptions = do
  argv  <- getArgs
  mopts <- interpretArgs $ "--threads=1" : argv
  case mopts of
    Left err       -> error err
    Right (opts,_) -> return opts

