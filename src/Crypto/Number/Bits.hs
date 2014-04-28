--
-- Bits.hs
--
-- Module Crypto.Number.Bits
--
-- Brandon Azad
--
-- Public Domain
--
--

-- |
-- This module provides several useful functions for working with numbers.
--

{-# LANGUAGE MagicHash #-}


module Crypto.Number.Bits
( bitLength
) where


import GHC.Types ( Int(..) )
import GHC.Integer.Logarithms ( integerLog2# )

import Data.Bits ()

bitLength :: Integer -> Int
bitLength n = I# (integerLog2# n) + 1


