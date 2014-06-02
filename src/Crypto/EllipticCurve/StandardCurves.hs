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
  -- * NIST P-256
    P256
  , p256
  , nistP256
  -- * NIST P-521
  , P521
  , p521
  , nistP521
  ) where

import Crypto.EllipticCurve
import Crypto.Number.Field
import Crypto.Number.Mod



------------------------------------------------------------------------------
-- NIST P-256
------------------------------------------------------------------------------


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
  , ecpBitLength = 256
  , ecpGenerator = fromAffine p256 $ Affine
      0x6b17d1f2e12c4247f8bce6e563a440f277037d812deb33a0f4a13945d898c296 -- x
      0x4fe342e2fe1a7f9b8ee7eb4a7c0f9e162bce33576b315ececbb6406837bf51f5 -- y
  , ecpOrder =
      0xffffffff00000000ffffffffffffffffbce6faada7179e84f3b9cac2fc632551 -- q
  , ecpCofactor = 1
  }


------------------------------------------------------------------------------
-- NIST P-521
------------------------------------------------------------------------------


-- | The field of integers modulo @p = 2^521 - 1@.
-- is the prime for the elliptic curve NIST P-521.
type P521 = Integer `Mod` 0x01ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff


-- | The elliptic curve NIST-P256 defined by the Weierstrass equation @y^2 =
-- x^3 + ax + b@ over the prime field 'P256'.
p521 :: EC Weierstrass P521
p521 = EC
  (Weierstrass
     0x01fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffc
     0x0051953eb9618e1c9a1f929a21a0b68540eea2da725b99b315f3b8b489918ef109e156193951ec7e937b1652c0bd3bb1bf073573df883d2c34f1ef451fd46b503f00)
  (ops ())


-- | The full definition of NIST-P256, including the generator, the order of
-- the subgroup, and the cofactor.
nistP521 :: (EllipticCurve Weierstrass p) => ECP Weierstrass p P521
nistP521 = ECP
  { ecpCurve = p521
  , ecpBitLength = 521
  , ecpGenerator = fromAffine p521 $ Affine
      0x00c6858e06b70404e9cd9e3ecb662395b4429c648139053fb521f828af606b4d3dbaa14b5e77efe75928fe1dc127a2ffa8de3348b3c1856a429bf97e7e31c2e5bd66
      0x011839296a789a3bc0045c8a5fb42c7d1bd998f54449579b446817afbd17273e662c97ee72995ef42640c550b9013fad0761353c7086a272c24088be94769fd16650
  , ecpOrder =
      0x1fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffa51868783bf2f966b7fcc0148f709a5d03bb5c9b8899c47aebb6fb71e91386409
  , ecpCofactor = 1
  }



