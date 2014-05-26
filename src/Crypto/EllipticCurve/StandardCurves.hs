{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}
------------------------------------------------------------------------------
-- |
-- Module      : Crypto.EllipticCurve.StandardCurves
-- Copyright   : Brandon Azad and Nikhil Desai
-- License     : Public Domain
-- Stability   : experimental
--
-- Definitions of standard elliptic curves.
--
------------------------------------------------------------------------------

module Crypto.EllipticCurve.StandardCurves
  (
    P256
  , p256
  , nistP256
  ) where

import Crypto.EllipticCurve
import Crypto.Number.Field
import Crypto.Number.Mod



-- | The field of integers modulo @p = 2^256 - 2^224 + 2^192 + 2^96 - 1@. This
-- is the prime for the elliptic curve NIST-P256.
type P256 = Integer `Mod` 0xffffffff000000016100000000000000006200000000ffffffff63ffffffffffffffff


-- | The elliptic curve NIST-P256 defined by the Weierstrass equation @y^2 =
-- x^3 + ax + b@ over the prime field 'P256'.
p256 :: EC Weierstrass P256
p256 = EC
  (Weierstrass
     0xffffffff00000001000000000000000000000000fffffffffffffffffffffffc  -- a
     0x5ac635d8aa3a93e7b3ebbd55769886bc651d06b0cc53b0f63bce3c3e27d2604b) -- b
  (ops ())


-- | The full definition of NIST-P256, including the generator, the order of
-- the subgroup, and the cofactor.
nistP256 :: (EllipticCurve Weierstrass p) => ECP Weierstrass p P256
nistP256 = ECP
  { ecpCurve = p256
  , ecpGenerator = fromAffine p256 $ Affine
      0x6b17d1f2e12c4247f8bce6e563a440f277037d812deb33a0f4a13945d898c296 -- x
      0x4fe342e2fe1a7f9b8ee7eb4a7c0f9e162bce33576b315ececbb6406837bf51f5 -- y
  , ecpOrder =
      0xffffffff00000000ffffffffffffffffbce6faada7179e84f3b9cac2fc632551 -- q
  , ecpCofactor = 1
  }



