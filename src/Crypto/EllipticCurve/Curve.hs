{-# LANGUAGE MultiParamTypeClasses #-}
------------------------------------------------------------------------------
-- |
-- Module      : Crypto.EllipticCurve.Curve
-- Copyright   : Brandon Azad and Nikhil Desai
-- License     : Public Domain
-- Stability   : experimental
--
-- Definitions of elliptic curve representations.
--
------------------------------------------------------------------------------

module Crypto.EllipticCurve.Curve
  (
    WeierstrassCurve(..)
  ) where

import Prelude hiding ( negate )
import Crypto.Number.Field hiding ( add, neg, sub, rep, mul, inv, div, pow )
import qualified Crypto.Number.Field as F
import Crypto.EllipticCurve.Type
import Crypto.EllipticCurve.Point
import Crypto.EllipticCurve.Group




-- | A Weierstrass representation of an elliptic curve, @y^2 = x^3 + ax + b@.
data WeierstrassCurve f = WeierstrassCurve
  { weierstrassA, weierstrassB :: f }
  deriving (Show)


-- | The definition for an elliptic curve in Weierstrass form
-- ('WeierstrassCurve') using the 'Affine' point representation.
instance EllipticCurve WeierstrassCurve Affine where

  onCurve (EC (WeierstrassCurve a b)
              (FieldOperations (+) _ _ _ (*) _ _ (^)))
          p
    | AffinePointAtInfinity <- p = True
    | (Affine x y)          <- p = y ^ 2 == (x ^ 3) + (a * x) + b

  add c@(EC (WeierstrassCurve a _)
            (FieldOperations (+) _ (-) (#) (*) _ (/) (^)))
      p1 p2
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

  negate (EC _ (FieldOperations { fneg = neg })) (Affine x y) =
    Affine x (neg y)

  -- Default implementation of 'multiply'.


-- | The Jacobian point @(X,Y,Z)@ over a Weierstrass curve represents the
-- point @(X/Z^2,Y/Z^3)@ in affine coordinates.
instance EllipticCurvePoint WeierstrassCurve Jacobian where

  toAffine (EC _ (FieldOperations _ _ _ _ (*) inv _ (^))) (Jacobian x y z)
    | z == zero = AffinePointAtInfinity
    | otherwise =
      let iz3 = inv (z ^ 3)
      in Affine (x * iz3 * z) (y * iz3)

  fromAffine _ (Affine x y)          = Jacobian x   y   one
  fromAffine _ AffinePointAtInfinity = Jacobian one one zero

  zeroPoint _ = (Jacobian one one zero)

  isZeroPoint _ (Jacobian _ _ z) = z == zero


-- | 
instance EllipticCurve WeierstrassCurve Jacobian where

  onCurve = undefined
  add = undefined
  negate = undefined




