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
import qualified Crypto.Number.Field as F
import Crypto.EllipticCurve.Type
import Crypto.EllipticCurve.Point
import Crypto.EllipticCurve.Group



-- | A Weierstrass representation of an elliptic curve.
-- y^2 = x^3 + ax + b
data WeierstrassCurve f = WeierstrassCurve
  { weierstrassA, weierstrassB :: f }
  deriving (Show)


-- |
instance EllipticCurvePoint WeierstrassCurve Jacobian where

  toAffine = undefined
  fromAffine = undefined


-- | 
instance EllipticCurve WeierstrassCurve Jacobian where

  add = undefined
  negate = undefined
  double = undefined




