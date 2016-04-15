{-# LANGUAGE CPP           #-}
{-# LANGUAGE TypeOperators #-}
-- |
-- Module      : Crypto.Lol.Cyclotomic.Tensor.Accelerate.Backend
-- Copyright   : [2016] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Crypto.Lol.Cyclotomic.Tensor.Accelerate.Backend
  where

import Data.Label
import System.Console.GetOpt


import Data.Array.Accelerate
import qualified Data.Array.Accelerate.Interpreter      as Interp
#ifdef ACCELERATE_CUDA_BACKEND
import qualified Data.Array.Accelerate.CUDA             as CUDA
#endif
#ifdef ACCELERATE_LLVM_NATIVE_BACKEND
import qualified Data.Array.Accelerate.LLVM.Native      as CPU
#endif
#ifdef ACCELERATE_LLVM_PTX_BACKEND
import qualified Data.Array.Accelerate.LLVM.PTX         as PTX
#endif


-- | The set of backends available to execute the program.
--
data Backend = Interpreter
#ifdef ACCELERATE_CUDA_BACKEND
             | CUDA
#endif
#ifdef ACCELERATE_LLVM_NATIVE_BACKEND
             | CPU
#endif
#ifdef ACCELERATE_LLVM_PTX_BACKEND
             | PTX
#endif
  deriving (Eq, Bounded)

-- The choice of show instance is important because this will be used to
-- generate the command line flag.
--
instance Show Backend where
  show Interpreter      = "interpreter"
#ifdef ACCELERATE_CUDA_BACKEND
  show CUDA             = "cuda"
#endif
#ifdef ACCELERATE_LLVM_NATIVE_BACKEND
  show CPU              = "llvm-cpu"
#endif
#ifdef ACCELERATE_LLVM_PTX_BACKEND
  show PTX              = "llvm-gpu"
#endif

-- The default backend to use. Currently the only complete accelerated backend
-- is CUDA, so default to that if it is available.
--
defaultBackend :: Backend
#ifdef ACCELERATE_CUDA_BACKEND
defaultBackend = CUDA
#else
defaultBackend = maxBound
#endif

-- The set of available backnds. This will be used for both the command line
-- options as well as the fancy header generation.
--
availableBackends :: (options :-> Backend) -> [OptDescr (options -> options)]
availableBackends optBackend =
  [ Option  [] [show Interpreter]
            (NoArg (set optBackend Interpreter))
            "reference implementation (sequential, slow)"
#ifdef ACCELERATE_CUDA_BACKEND
  , Option  [] [show CUDA]
            (NoArg (set optBackend CUDA))
            "implementation for NVIDIA GPUs (parallel)"
#endif
#ifdef ACCELERATE_LLVM_NATIVE_BACKEND
  , Option  [] [show CPU]
            (NoArg (set optBackend CPU))
            "LLVM based implementation for multicore CPUs (parallel)"
#endif
#ifdef ACCELERATE_LLVM_PTX_BACKEND
  , Option  [] [show PTX]
            (NoArg (set optBackend PTX))
            "LLVM based implementation for NVIDIA GPUs (parallel)"
#endif
  ]

-- | Execute an Accelerate program
--
run :: Arrays a => Acc a -> a
run = runWith defaultBackend

-- | Execute an Accelerate program of one argument
--
run1 :: (Arrays a, Arrays b) => (Acc a -> Acc b) -> a -> b
run1 = run1With defaultBackend


-- | Execute Accelerate programs using the selected backend
--
runWith :: Arrays a => Backend -> Acc a -> a
runWith Interpreter = Interp.run
#ifdef ACCELERATE_CUDA_BACKEND
runWith CUDA        = CUDA.run
#endif
#ifdef ACCELERATE_LLVM_NATIVE_BACKEND
runWith CPU         = CPU.run
#endif
#ifdef ACCELERATE_LLVM_PTX_BACKEND
runWith PTX         = PTX.run
#endif


run1With :: (Arrays a, Arrays b) => Backend -> (Acc a -> Acc b) -> a -> b
run1With Interpreter f = Interp.run1 f
#ifdef ACCELERATE_CUDA_BACKEND
run1With CUDA        f = CUDA.run1 f
#endif
#ifdef ACCELERATE_LLVM_NATIVE_BACKEND
run1With CPU         f = CPU.run1 f
#endif
#ifdef ACCELERATE_LLVM_PTX_BACKEND
run1With PTX         f = PTX.run1 f
#endif

