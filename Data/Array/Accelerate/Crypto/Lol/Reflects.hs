{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Array.Accelerate.Crypto.Lol.Reflects (

  Reflects(..)

) where

import Data.Array.Accelerate                                        as A
import Crypto.Lol.Reflects

instance (Reflects a i, Elt i) => Reflects a (Exp i) where
  value = constant <$> value

