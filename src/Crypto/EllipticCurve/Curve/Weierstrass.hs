{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
------------------------------------------------------------------------------
-- |
-- Module      : Crypto.EllipticCurve.Curve.Weierstrass
-- Copyright   : Brandon Azad and Nikhil Desai
-- License     : Public Domain
-- Stability   : experimental
--
-- Definitions of Weierstrass curve operations.
--
------------------------------------------------------------------------------

module Crypto.EllipticCurve.Curve.Weierstrass
  (
    Weierstrass(..)
  ) where

import Prelude hiding ( negate )
import Crypto.Number.Field hiding ( add, neg, sub, rep, mul, inv, div, pow )
import qualified Crypto.Number.Field as F
import Crypto.EllipticCurve.Type
import Crypto.EllipticCurve.Point
import Crypto.EllipticCurve.Group



------------------------------------------------------------------------------
-- Affine
------------------------------------------------------------------------------


-- | A Weierstrass representation of an elliptic curve, @y^2 = x^3 + ax + b@.
data Weierstrass f = Weierstrass
  { weierstrassA, weierstrassB :: f }
  deriving (Show)


-- | The definition for an elliptic curve in Weierstrass form
-- ('Weierstrass') using the 'Affine' point representation.
instance EllipticCurve Weierstrass Affine where

  onCurve (EC (Weierstrass a b) FieldOperations {..}) p
    | AffinePointAtInfinity <- p = True
    | (Affine x y)          <- p = y ^ 2 == (x ^ 3) + (a * x) + b

  add c@(EC (Weierstrass a _) FieldOperations {..}) p1 p2
    | isZeroPoint c p1 = p2
    | isZeroPoint c p2 = p1
    | (Affine x1 y1) <- p1, (Affine x2 y2) <- p2 =
      let (dx, dy) = (x2 - x1, y2 - y1)
          add' s =
            let x3 = (s ^ 2) - x2 - x1
                y3 = s * (x1 - x3) - y1
            in Affine x3 y3
      in if dx == zero
           then if dy == zero
                  then add' $ (3 # (x1 ^ 2) + a) / (2 # y1)
                  else zeroPoint c
           else add' $ dy / dx

  double c@(EC (Weierstrass a _) FieldOperations {..}) p
    | isZeroPoint c p = p
    | (Affine x1 y1) <- p =
      let s  = (3 # (x1 ^ 2) + a) / (2 # y1)
          x3 = (s^2) - (2 # x1)
          y3 = s * (x1 - x3) - y1
      in Affine x3 y3

  negate (EC _ FieldOperations {..}) (Affine x y) = Affine x ((.-) y)
  negate _ AffinePointAtInfinity = AffinePointAtInfinity

  -- Default implementation of 'multiply'.


------------------------------------------------------------------------------
-- Jacobian
------------------------------------------------------------------------------


-- | The Jacobian point @(X,Y,Z)@ over a Weierstrass curve represents the
-- point @(X/Z^2,Y/Z^3)@ in affine coordinates.
instance EllipticCurvePoint Weierstrass Jacobian where

  toAffine (EC _ FieldOperations {..}) (Jacobian x y z)
    | z == zero = AffinePointAtInfinity
    | otherwise =
      let iz3 = (./) (z ^ 3)
      in Affine (x * iz3 * z) (y * iz3)

  fromAffine _ (Affine x y)          = Jacobian x   y   one
  fromAffine _ AffinePointAtInfinity = Jacobian one one zero

  zeroPoint _ = (Jacobian one one zero)

  isZeroPoint _ (Jacobian _ _ z) = z == zero


-- | The definition for Weierstrass curve groups in Jacobian form.
instance EllipticCurve Weierstrass Jacobian where

  onCurve (EC (Weierstrass a b) FieldOperations {..}) (Jacobian x y z) =
    let z2 = z^2
        z4 = z2^2
        z6 = z2 * z4
    in y^2 == (x^3) + (a * x * z4) + (b * z6)

  add c@(EC _ FieldOperations {..})
      p1@(Jacobian x1 y1 z1) p2@(Jacobian x2 y2 z2)
    | z1 == zero = p2
    | z2 == zero = p1
    | otherwise = jacobianAdd c x1 y1 z1 x2 y2 z2

  double c@(EC _ FieldOperations {..}) p@(Jacobian x1 y1 z1)
    | z1 == zero = p
    | otherwise  = jacobianDouble' c x1 y1 z1 (z1^2)

  negate c@(EC _ FieldOperations {..}) p@(Jacobian x1 y1 z1) =
    Jacobian x1 ((.-) y1) z1


-- | Internal Jacobian adding routine.
jacobianAdd :: (Field f)
             => EC Weierstrass f -> f -> f -> f -> f -> f -> f
             -> Jacobian Weierstrass f
jacobianAdd c@(EC _ FieldOperations {..}) x1 y1 z1 x2 y2 z2 =
  let z1_2 = z1^2                             -- 1S
      z2_2 = z2^2                             -- 1S
      u1   = x1 * z2_2                        -- 1M
      u2   = x2 * z1_2                        -- 1M
      h    = u2 - u1                          -- 1A
      s1   = (y1 * z2) * z2_2                 -- 2M
      s2   = (y2 * z1) * z1_2                 -- 2M
      q    = s2 - s1                          -- 1A
  in if h == zero           -- affine: x1 == x2
       then if q == zero    -- affine: y1 == y2
              then jacobianDouble' c x1 y1 z1 z1_2
              else zeroPoint c
       else jacobianAdd' c z1 z1_2 z2 z2_2 u1 h s1 q


-- | Internal Jacobian adding routine.
jacobianAdd' :: (Field f)
             => EC Weierstrass f -> f -> f -> f -> f -> f -> f -> f -> f
             -> Jacobian Weierstrass f
jacobianAdd' c@(EC _ FieldOperations {..}) z1 z1_2 z2 z2_2 u1 h s1 q =
  let i    = (2 # h)^2                        -- 1S, 1(*2)
      j    = h * i                            -- 1M
      r    = 2 # q                            -- 1(*2)
      v    = u1 * i                           -- 1M
      x3   = (r^2) - j - (2 # v)              -- 1S, 1(*2), 2A
      y3   = (r * (v - x3)) - (2 # (s1 * j))  -- 2M, 1(*2), 2A
      z3   = ((z1 + z2)^2 - z1_2 - z2_2) * h  -- 1M, 1S, 3A
  in Jacobian x3 y3 z3


-- | Internal Jacobian doubling routine.
jacobianDouble' :: (Field f)
               => EC Weierstrass f -> f -> f -> f -> f
               -> Jacobian Weierstrass f
jacobianDouble' (EC (Weierstrass a _) FieldOperations {..}) x1 y1 z1 z1_2 =
  let x1_2 = x1^2                                 -- 1S
      y1_2 = y1^2                                 -- 1S
      y1_4 = y1_2^2                               -- 1S
      s    = 2 # ((x1 + y1_2)^2 - x1_2 - y1_4)    -- 1S, 1(*2), 3A
      m    = (3 # x1_2) + (a * (z1_2^2))          -- 1S, 1(*a), 1(*3), 1A
      x3   = (m^2) - (2 # s)                      -- 1S, 1(*2), 1A
      y3   = (m * (s - x3)) - (8 # y1_4)          -- 1M, 1(*8), 2A
      z3   = (y1 + z1)^2 - y1_2 - z1_2            -- 1S, 3A
  in Jacobian x3 y3 z3                            -- 1M, 1(*a), 7S, 2(*2),
                                                  -- 1(*3), 1(*8), 10A


