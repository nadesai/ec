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




-- | A Weierstrass representation of an elliptic curve.
-- y^2 = x^3 + ax + b
data WeierstrassCurve f = WeierstrassCurve
  { weierstrassA, weierstrassB :: f }
  deriving (Show)


-- | The definition for an elliptic curve in Weierstrass form
-- ('WeierstrassCurve') using the 'Affine' point representation.
instance EllipticCurve WeierstrassCurve Affine where

  add (EC wc (FieldOperations (+) neg (-) (#) (*) inv (/) (^)))
      p1@(Affine x1 y1) p2@(Affine x2 y2) =
    let s = (y2 - y1) / (x2 - x1)
        x3 = s ^ 2 - x2 - x1
        y3 = s * (x1 - x3) - y1
    in Affine x3 y3

  double (EC wc (FieldOperations (+) neg (-) (#) (*) inv (/) (^)))
         p@(Affine x y) =
    let s = (3 # (x^2) + weierstrassA wc) / (2 # y)
        x2 = s ^ 2 - (2 # x)
        y2 = s * (x - x2) - y
    in Affine x2 y2

  negate (EC wc (FieldOperations { fneg = neg })) (Affine x y) =
    Affine x (neg y)

-- |
instance EllipticCurvePoint WeierstrassCurve Jacobian where

  toAffine = undefined
  fromAffine = undefined


-- | 
instance EllipticCurve WeierstrassCurve Jacobian where

  add = undefined
  negate = undefined
  double = undefined




