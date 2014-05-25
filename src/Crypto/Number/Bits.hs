{-# LANGUAGE MagicHash #-}
------------------------------------------------------------------------------
-- |
-- Module      : Crypto.Number.Bits
-- Copyright   : Brandon Azad
-- License     : Public Domain
-- Stability   : experimental
--
-- Bitwise manipulation of integers.
--
------------------------------------------------------------------------------

module Crypto.Number.Bits
  (
    bitLength
  , module Data.Bits
  ) where



import GHC.Types              ( Int(..) )
import GHC.Integer.Logarithms ( integerLog2# )

import Data.Bits

bitLength :: Integer -> Int
bitLength n = I# (integerLog2# n) + 1


