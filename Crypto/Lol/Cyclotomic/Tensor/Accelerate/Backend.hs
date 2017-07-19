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
import Data.List
import System.Console.GetOpt
import System.Environment
import System.IO.Unsafe
import Text.PrettyPrint

import Data.Array.Accelerate                            ( Acc, Arrays )
import qualified Data.Array.Accelerate.Interpreter      as Interp
#ifdef ACCELERATE_LLVM_NATIVE_BACKEND
import qualified Data.Array.Accelerate.LLVM.Native      as CPU
#endif
#ifdef ACCELERATE_LLVM_PTX_BACKEND
import qualified Data.Array.Accelerate.LLVM.PTX         as PTX
#endif


-- | The set of backends available to execute the program.
--
data Backend = Interpreter
#ifdef ACCELERATE_LLVM_NATIVE_BACKEND
             | CPU
#endif
#ifdef ACCELERATE_LLVM_PTX_BACKEND
             | PTX
#endif
  deriving (Eq, Enum, Bounded)

-- The choice of show instance is important because this will be used to
-- generate the command line flag.
--
instance Show Backend where
  show Interpreter      = "interpreter"
#ifdef ACCELERATE_LLVM_NATIVE_BACKEND
  show CPU              = "llvm-cpu"
#endif
#ifdef ACCELERATE_LLVM_PTX_BACKEND
  show PTX              = "llvm-ptx"
#endif

-- The set of available backends. This will be used for both the command line
-- options as well as the fancy header generation.
--
availableBackends :: (options :-> Backend) -> [OptDescr (options -> options)]
availableBackends optBackend =
  [ Option  [] [show Interpreter]
            (NoArg (set optBackend Interpreter))
            "reference implementation (sequential, slow)"
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


defaultBackend :: Backend
defaultBackend =
  case maxBound of
    Interpreter -> Interpreter
#if defined(ACCELERATE_LLVM_NATIVE_BACKEND) || defined(ACCELERATE_LLVM_PTX_BACKEND)
    _           -> succ Interpreter
#endif

{-# NOINLINE theBackend #-}
theBackend :: Backend
theBackend = unsafePerformIO $ do
  mb <- lookupEnv "LOL_ACCELERATE_BACKEND"
  case mb of
    Nothing   -> return defaultBackend
    Just this ->
      case filter (\backend -> this `isPrefixOf` show backend) [minBound .. maxBound] of
        []        -> unknown
        [backend] -> return backend
        alts      -> case find (\backend -> this == show backend) alts of
                       Just backend -> return backend
                       Nothing      -> ambiguous alts
      where
        unknown         = error . render $ text "lol-accelerate: Unknown backend:" <+> quotes (text this)
        ambiguous alts  = error . render $
          vcat [ text "lol-accelerate: Ambiguous backend:" <+> quotes (text this)
               , text ""
               , text "Did you mean one of these?"
               , nest 4 $ vcat (map (text . show) alts)
               ]


-- | Execute an Accelerate program
--
-- TODO: How to select which backend to use? We might need to use an environment
--       variable or command line flag, as it looks like there is no single
--       (top-level) point at which we execute the entire Lol computation.
--
run :: Arrays a => Acc a -> a
run = runWith theBackend

-- | Execute an Accelerate program of one argument
--
run1 :: (Arrays a, Arrays b) => (Acc a -> Acc b) -> a -> b
run1 = run1With theBackend


-- | Execute Accelerate programs using the selected backend
--
runWith :: Arrays a => Backend -> Acc a -> a
runWith Interpreter = Interp.run
#ifdef ACCELERATE_LLVM_NATIVE_BACKEND
runWith CPU         = CPU.run
#endif
#ifdef ACCELERATE_LLVM_PTX_BACKEND
runWith PTX         = PTX.run
#endif


run1With :: (Arrays a, Arrays b) => Backend -> (Acc a -> Acc b) -> a -> b
run1With Interpreter f = Interp.run1 f
#ifdef ACCELERATE_LLVM_NATIVE_BACKEND
run1With CPU         f = CPU.run1 f
#endif
#ifdef ACCELERATE_LLVM_PTX_BACKEND
run1With PTX         f = PTX.run1 f
#endif

