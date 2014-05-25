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

  add c@(EC (WeierstrassCurve a b)
            (FieldOperations (+) neg (-) (#) (*) inv (/) (^)))
      p1 p2
    | isZeroPoint c p1 = p2
    | isZeroPoint c p2 = p1
    | (Affine x1 y1) <- p1, (Affine x2 y2) <- p2 =
      let (dx, dy) = (x2 - x1, y2 - y1)
      in if dx == zero
           then if dy == zero
                  then let s  = (3 # (x1 ^ 2) + a) / (2 # y1)
                           x3 = (s ^ 2) - (2 # x1)
                           y3 = s * (x1 - x3) - y1
                       in Affine x3 y3
                  else zeroPoint c
           else let s = dy / dx
                    x3 = (s ^ 2) - x2 - x1
                    y3 = s * (x1 - x3) - y1
                in Affine x3 y3

  negate (EC _ (FieldOperations { fneg = neg })) (Affine x y) =
    Affine x (neg y)

-- |
instance EllipticCurvePoint WeierstrassCurve Jacobian where

  toAffine = undefined
  fromAffine = undefined


-- | 
instance EllipticCurve WeierstrassCurve Jacobian where

  add = undefined
  negate = undefined




