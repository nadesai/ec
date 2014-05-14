------------------------------------------------------------------------------
-- |
-- Module      : Crypto.Number.Prime
-- Copyright   : Brandon Azad
-- License     : Public Domain
-- Stability   : experimental
--
-- This Haskell library provides an interface for testing whether a number is
-- prime and for generating prime numbers.
--
------------------------------------------------------------------------------

module Crypto.Number.Prime
  (

  -- * Primality testing

    probablyPrime

  -- ** Primality primitives

  , millerRabin

  -- * Prime generation

  ) where

import Crypto.Number.Modular ( expm )
import Data.List             ( mapAccumL )



-- | Computes single iteration of the Miller-Rabin probabilistic primality
-- test using the given witness.
millerRabin :: Integer -> Integer -> Bool
millerRabin a n
  | null ys || head ys == 1 = True
  | otherwise               = 1 `notElem` ys && length ys < s
  where
    n1 = n - 1
    split2 s r
      | odd r = (s, r)
      | otherwise = split2 (s + 1) (r `div` 2)
    (s, r) = split2 0 n1
    ys = takeWhile (/= n1) . take s . iterate (\y -> y^2 `mod` n) $ expm a r n


-- | Determine whether the number @n@ is composite or probably prime.
-- Internally, this is done using the Miller-Rabin primality test.
-- If the Miller-Rabin test determines that @n@ is probably prime, the
-- probability that @n@ is actually composite is at most @(1/4)^t@.
probablyPrime :: ((Integer, Integer) -> g -> (Integer, g))
                 -- ^ A random integer generator with the required security
                 -- properties.
              -> g
                 -- ^ An initial random generator state.
              -> Int
                 -- ^ The desired probability threshhold for which an integer
                 -- is considered probably prime.
              -> Integer
                 -- ^ The integer to test for primality.
              -> (Bool, g)
                 -- ^ Whether the integer is probably prime and the new
                 -- generator state.
probablyPrime rgen g t n
  | any (\p -> n `rem` p == 0) . takeWhile (\p -> p^2 <= n) $ smallPrimes
                         = (False, g)
  | n `elem` smallPrimes = (True, g)
  | otherwise            = (prime, g')
  where
    prime = all (flip millerRabin n) as
    (g', as) = accumGen (swap . rgen (2, n-2)) g t
    accumGen f s n = mapAccumL (\acc () -> f acc) s $ replicate n ()
    swap (x,y) = (y,x)


-- TODO: To generate a secure random prime, we need some notion of generating
-- secure random bits, which RandomGen does not provide.
{-

-- | Generate a random prime given a generator @g@, a probability threshold
-- @t@, and an inclusive range @r@.
randomPrime :: (RandomGen g) => g -> Int -> (Integer, Integer) -> (Integer, g)
randomPrime g t r = go $ gen g
  where
    r' = let (a, b) = r in (a, if odd b then b else b-1)
    gen g = let (n, g') = randomR r g in (n `setBit` 0, g')
    go (n, g)
      | prime     = (n, g')
      | otherwise = go $ gen g'
      where
        (prime, g') = probablyPrime g t n

-}


-- | A list of small prime numbers.
smallPrimes :: [Integer]
smallPrimes = takeWhile (<1024) primes
  where
    primes = 2 : filter isPrime [3,5..]
    isPrime p = all (\d -> p `rem` d /= 0) $ takeWhile (\d -> d^2 <= p) primes


