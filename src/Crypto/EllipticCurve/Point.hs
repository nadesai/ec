{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
------------------------------------------------------------------------------
-- |
-- Module      : Crypto.EllipticCurve.Point
-- Copyright   : Brandon Azad and Nikhil Desai
-- License     : Public Domain
-- Stability   : experimental
--
-- Definitions of elliptic curve points.
--
------------------------------------------------------------------------------

module Crypto.EllipticCurve.Point
  (

  -- * Elliptic curve point type

    EllipticCurvePoint(..)

  -- * Point representations

  , Affine(..)
  , Jacobian(..)

  ) where

import Crypto.Number.Field
import Crypto.EllipticCurve.Type



class EllipticCurvePoint c p where

  -- | Convert the point to affine representation.
  toAffine        :: (Field f) => EC c f -> p c f -> Affine c f

  -- | Convert the point from affine representation to an efficient point
  -- representation for curve arithmetic.
  fromAffine      :: (Field f) => EC c f -> Affine c f -> p c f

  -- | The zero point on an elliptic curve, which is the point at infinity in
  -- affine representation, is the identity element of the elliptic curve
  -- group.
  zeroPoint       :: (Field f) => EC c f -> p c f
  zeroPoint c = fromAffine c AffinePointAtInfinity

  -- | Efficiently test whether or not a point is the zero point.
  isZeroPoint     :: (Field f) => EC c f -> p c f -> Bool
  isZeroPoint c p = toAffine c p == AffinePointAtInfinity


-- | Affine representation of a point.  This is the simplest representation in
-- which the @x@- and @y@-coordinates of the point are stored explicitly.
-- Unfortunately, most elliptic curve representations require a field
-- inversion to add points under the affine representation, which makes it
-- less efficient than other point representations for elliptic curve point
-- multiplication.
data Affine (c :: * -> *) f = Affine { affineX, affineY :: f }
                            | AffinePointAtInfinity
                            deriving (Eq, Show)


instance EllipticCurvePoint c Affine where

  toAffine   _ p = p
  fromAffine _ p = p
  zeroPoint  _ = AffinePointAtInfinity
  isZeroPoint _ AffinePointAtInfinity = True
  isZeroPoint _ _                     = False


-- | Jacobian @(X,Y,Z)@ representation of a point.
-- For Weierstrass curves, this point represents the affine point
-- @(X/Z^2,Y/Z^3)@.
data Jacobian (c :: * -> *) f = Jacobian
  { jacobianX, jacobianY, jacobianZ :: f }
  deriving (Show)

-- Instances of @Jacobian@ are provided by specific curves in the module
-- @Crypto.EllipticCurve.Curve@.


