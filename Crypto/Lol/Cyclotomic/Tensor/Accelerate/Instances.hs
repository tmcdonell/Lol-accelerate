{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fplugin=Data.Array.Accelerate.LLVM.Native.Plugin #-}
-- |
-- Module      : Crypto.Lol.Cyclotomic.Tensor.Accelerate.Instances
-- Copyright   : [2016] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Crypto.Lol.Cyclotomic.Tensor.Accelerate.Instances (

  Proxy(..),
  Dispatch(..),
  module Crypto.Lol.Cyclotomic.Tensor.Accelerate.TH,

) where

import Data.Proxy
import Crypto.Lol.Types

import Data.Array.Accelerate.Crypto.Lol.Types.Complex               ()

import Crypto.Lol.Cyclotomic.Tensor.Accelerate.Backend
import Crypto.Lol.Cyclotomic.Tensor.Accelerate.Prim
import Crypto.Lol.Cyclotomic.Tensor.Accelerate.TH

#include "cbits/instances.h"

-- GHC bug prevents the following from working??
-- Just copy/paste the result to be inserted below... |:
--
-- To generate:
--
-- > cpphs -I. Crypto/Lol/Cyclotomic/Tensor/Accelerate/Instances.hs
--
-- mkDispatch(Double)
-- mkDispatch(Complex Double)
--
; instance Dispatch (Double) where                            ;   expose'   = $( runQ (_expose   (Proxy::Proxy (Double))) ) ;   unexpose' = $( runQ (_unexpose (Proxy::Proxy (Double))) ) ;                                                         ;   eq        = $( runQ (_eq  (Proxy::Proxy (Double))) )      ;   neq       = $( runQ (_neq (Proxy::Proxy (Double))) )      ;                                                         ;   pL'       = $( runQ (_pL    (Proxy::Proxy (Double))) )    ;   pLInv'    = $( runQ (_pLInv (Proxy::Proxy (Double))) )    ;   pGPow'    = $( runQ (_pGPow (Proxy::Proxy (Double))) )    ;   pGDec'    = $( runQ (_pGDec (Proxy::Proxy (Double))) )    ;                                                         ;   embedPow' = $( runQ (_embedPow (Proxy::Proxy (Double))) )
; instance Dispatch (Complex Double) where                            ;   expose'   = $( runQ (_expose   (Proxy::Proxy (Complex Double))) ) ;   unexpose' = $( runQ (_unexpose (Proxy::Proxy (Complex Double))) ) ;                                                         ;   eq        = $( runQ (_eq  (Proxy::Proxy (Complex Double))) )      ;   neq       = $( runQ (_neq (Proxy::Proxy (Complex Double))) )      ;                                                         ;   pL'       = $( runQ (_pL    (Proxy::Proxy (Complex Double))) )    ;   pLInv'    = $( runQ (_pLInv (Proxy::Proxy (Complex Double))) )    ;   pGPow'    = $( runQ (_pGPow (Proxy::Proxy (Complex Double))) )    ;   pGDec'    = $( runQ (_pGDec (Proxy::Proxy (Complex Double))) )    ;                                                         ;   embedPow' = $( runQ (_embedPow (Proxy::Proxy (Complex Double))) )

