{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
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

  toAffine   :: (Field f) => EC c f -> p c f -> Affine c f

  fromAffine :: (Field f) => EC c f -> Affine c f -> p c f


-- | Affine representation of a point.
data Affine (c :: * -> *) f = Affine { affineX, affineY :: f }
                            | AffinePointAtInfinity
                            deriving (Eq, Show)


instance EllipticCurvePoint c Affine where

  toAffine   _ p = p
  fromAffine _ p = p


-- | Jacobian @(X,Y,Z)@ representation of a point.
data Jacobian (c :: * -> *) f = Jacobian
  { jacobianX, jacobianY, jacobianZ :: f }
  deriving (Show)

-- Instances of @Jacobian@ are provided by specific curves in
-- Crypto.EllipticCurve.Curve.


