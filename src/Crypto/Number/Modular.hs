--
-- Modular.hs
--
-- Module Crypto.Number.Modular
--
-- Brandon Azad
--
-- Public Domain
--
--

-- |
-- Modular arithmetic.
--


module Crypto.Number.Modular
( expm
, expm2k
, invm
, invm'
, divm'
, egcd
, egcd'
, crt
) where

import Data.Bits ( Bits, (.&.) )


-- Run the exponentiation algorithm with the given operation.
-- Derived from the source of Prelude.(^).
-- This implementation is not resistant to timing attacks.
gexp1 :: (Integral n) => (a -> a -> a) -> a -> n -> a
gexp1 (*%) a n = go2 a n
  where
    go2 a n
      | even n    = go2 (a *% a) (n `quot` 2)
      | n == 1    = a
      | otherwise = go3 (a *% a) a ((n - 1) `quot` 2)
    go3 a r n
      | even n    = go3 (a *% a) r (n `quot` 2)
      | n == 1    = (a *% r)
      | otherwise = go3 (a *% a) (r *% a) ((n - 1) `quot` 2)


-- | Computes a^n `mod` m.
expm :: (Integral a, Integral n) => a -> a -> n -> a
expm m a n
  | m <= 0    = error "modulus must be positive"
  | n < 0     = error "negative exponent"
  | n == 0    = 1 `mod` m
  | otherwise = gexp1 (\a b -> (a * b) `mod` m) a n


-- Computes a^n `mod` 2^k - 1. This can be more efficient than expm.
expm2k :: (Bits a, Integral a, Integral n) => a -> a -> n -> a
expm2k k a n
  | k <= 0    = error "modulus must be positive"
  | n < 0     = error "negative exponent"
  | n == 0    = 1 `mod` m
  | otherwise = gexp1 (\a b -> (a * b) .&. m) a n
  where
    m = 2^k - 1


-- | Computes b such that a * b `mod` m == 1.
invm :: (Integral a) => a -> a -> Maybe a
invm m a = if g == 1 then Just b' else Nothing
  where
    (g, b, _) = egcd' (a `mod` m) m
    b' = if b < 0 then b + m else b


-- | The same as invm, but assumes that the inverse always exists.
invm' :: (Integral a) => a -> a -> a
invm' m a = case invm m a of
  Just b  -> b
  Nothing -> error "inverse does not exist"


-- | Computes a * invm' m b.
divm' :: (Integral a) => a -> a -> a -> a
divm' m a b = a * invm' m b


-- | Computes the Extended Euclidean Algorithm on a and b, returning a tuple
-- (gcd(a,b),x,y) such that ax + by = gcd(a,b).
egcd :: (Integral a) => a -> a -> (a, a, a)
egcd a b
  | g < 0     = (-g, -x, -y)
  | otherwise = (g, x, y)
  where
    (g, x, y) = egcd' a b


-- | Computes the Extended Euclidean Algorithm, but the greatest common
-- divisor might be negative.
egcd' :: (Integral a) => a -> a -> (a, a, a)
egcd' a b = go a b 1 0 0 1
  where
    go a 0 x0 y0 _  _  = (a, x0, y0)
    go a b x0 y0 x1 y1 = go b r x1 y1 (x0 - q * x1) (y0 - q * y1)
      where (q, r) = a `divMod` b


-- | Computes the Chinese Remainder Theorem on the system x ≡ cᵢ (mod mᵢ).
crt :: (Integral a) => [(a, a)] -> Maybe (a, a)
crt cms = do
  ts <- sequence $ map (uncurry term) cms
  return $ ((sum ts) `mod` m, m)
  where
    m = product . map snd $ cms
    term ci mi = do
      let mmi = m `div` mi
      inv <- mmi `invm` mi
      return $ mmi * inv * ci



