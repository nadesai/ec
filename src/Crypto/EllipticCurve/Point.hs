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


import qualified Crypto.Number.Field as F
import Crypto.EllipticCurve.Group
import Crypto.EllipticCurve.Curve



class EllipticCurvePoint c p where

  toAffine   :: (F.Field f, EllipticCurve c p)
             => EC c f -> p c f -> Affine c f

  fromAffine :: (F.Field f, EllipticCurve c p)
             => EC c f -> Affine c f -> p c f


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



