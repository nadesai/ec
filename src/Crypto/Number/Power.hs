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

    expsq
  , expsq'

  -- * Timing attack resistant exponentiation

  , montgomery
  , montgomery'

  ) where

import Crypto.Number.Bits



-- Convert a specialized monoidal power operation into a generalized group
-- power using the given inversion function and identity element.
gpow :: (a -> Integer -> a) -> (a -> a) -> a -> a -> Integer -> a
gpow pow inv id a n
  | n < 0     = pow (inv a) (negate n)
  | n == 0    = id
  | otherwise = pow      a          n


-- | Fast general group exponentiation by multiplication and squaring. The
-- implementation is based off of Prelude.(^).
-- @expsq mul sqr inv id a n@ computes @a^n@ in the group with group operation
-- @mul@, (potentially optimized) self-operation @sqr@, inverse @inv@, and
-- identity @id@.
expsq :: (a -> a -> a) -> (a -> a) -> (a -> a) -> a -> a -> Integer -> a
expsq mul sqr = gpow expsq'
  where
    expsq' a n
      | even n    = expsq'  (sqr a) ( n      `quot` 2)
      | n == 1    = a
      | otherwise = expsq'' (sqr a) ((n - 1) `quot` 2) a
    expsq'' a n b
      | even n    = expsq'' (sqr a) ( n      `quot` 2) b
      | n == 1    = a `mul` b
      | otherwise = expsq'' (sqr a) ((n - 1) `quot` 2) (a `mul` b)


-- | Similar to 'expsq', except the self-operation in the group defaults to
-- the group operation.
expsq' :: (a -> a -> a) -> (a -> a) -> a -> a -> Integer -> a
expsq' mul = expsq mul (\a -> a `mul` a)


-- | Montgomery multiplication.
-- @montgomery mul sqr inv id n a d@ computes @a^d@ in the group with operation
-- @mul@, a (potentially optimized) self-operation @sqr@, inverse @inv@, and
-- identity @id@, but always performing exactly @n@ iterations to help minimize
-- information leakage through timing channels. @n@ should usually be the bit
-- length of the maximum possible value of @d@.
montgomery :: (a -> a -> a) -> (a -> a) -> (a -> a) -> a -> Int
           -> a -> Integer -> a
montgomery mul sqr inv id n a d = gpow montgomery' inv id a d
  where
    montgomery' a d = go id a n
      where
        go r0 r1 n
          | n == -1     = (r0 `seq` r1) `seq` r0
          | testBit d n = go (r0 `mul` r1) (sqr r0)      (n - 1)
          | otherwise   = go (sqr r0)      (r0 `mul` r1) (n - 1)


-- | Similar to 'montgomery', except the self-operation in the group defaults
-- to the group operation.
montgomery' :: (a -> a -> a) -> (a -> a) -> a -> Int
           -> a -> Integer -> a
montgomery' mul = montgomery mul (\a -> a `mul` a)

