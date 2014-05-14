------------------------------------------------------------------------------
-- |
-- Module      : Crypto.Number.Power
-- Copyright   : Brandon Azad and Nikhil Desai
-- License     : Public Domain
-- Stability   : experimental
--
-- Various exponentiation algorithms over groups.
--
------------------------------------------------------------------------------


module Crypto.Number.Power
  (

  -- * Fast exponentiation

    sqpow

  -- * Timing attack resistant exponentiation

  , montgomery

  ) where



-- Convert a specialized monoidal power operation into a generalized group
-- power using the given inversion function and identity element.
gpow :: ((a -> a -> a) -> a -> Integer -> a)
     -> (a -> a -> a) -> (a -> a) -> a -> a -> Integer -> a
gpow pow op inv id a n
  | n < 0     = pow op (inv a) (negate n)
  | n == 0    = id
  | otherwise = pow op      a          n


-- | Fast general group exponentiation by squaring. The implementation is
-- based off of Prelude.(^).
sqpow :: (a -> a -> a) -> (a -> a) -> a -> a -> Integer -> a
sqpow = gpow sqpow'
  where
    sqpow' op = go1
      where
        go1 a n
          | even n    = go1 (a `op` a) (n `quot` 2)
          | n == 1    = a
          | otherwise = go2 (a `op` a) ((n - 1) `quot` 2) a
        go2 a n b
          | even n    = go2 (a `op` a) (n `quot` 2) b
          | n == 1    = a `op` b
          | otherwise = go2 (a `op` a) ((n - 1) `quot` 2) (a `op` b)


-- | Montgomery multiplication.
montgomery :: (a -> a -> a) -> (a -> a) -> a -> a -> Integer -> a
montgomery = undefined



