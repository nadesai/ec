{-# LANGUAGE MultiParamTypeClasses #-}
------------------------------------------------------------------------------
-- |
-- Module      : Crypto.EllipticCurve.Group
-- Copyright   : Brandon Azad and Nikhil Desai
-- License     : Public Domain
-- Stability   : experimental
--
-- Definitions of elliptic curve groups and related operations.
--
------------------------------------------------------------------------------

module Crypto.EllipticCurve.Group
  (

    -- Elliptic curve type
    EllipticCurve(..)

  ) where

import Prelude hiding ( negate )

import Crypto.Number.Field hiding ( add )
import Crypto.Number.Power
import Crypto.EllipticCurve.Type
import Crypto.EllipticCurve.Point



-- | The class @EllipticCurve c p@ represents a formulation of a group law of
-- the curve @c@ over a point representation @p@. While all elliptic curves
-- @c@ should implement the same group law, the parameter @p@ allows different
-- representations of the curve to have different implementations.
class (EllipticCurvePoint c p) => EllipticCurve c p where

  -- | @onCurve c p@ returns @True@ if and only if the given point is on the
  -- elliptic curve.
  onCurve  :: (Field f) => EC c f -> p c f -> Bool

  -- | Add two points on the elliptic curve. Since elliptic curve point
  -- addition formulae are not usually unified, the add method will usually
  -- have to identify when the points are equivalent and perform a doubling
  -- operation instead.
  add      :: (Field f) => EC c f -> p c f -> p c f -> p c f

  -- | Negate a point on the elliptic curve.
  negate   :: (Field f) => EC c f -> p c f -> p c f

  -- | @multiply c n d p@ computes the @d@th multiple of the elliptic curve
  -- point @p@, that is, @p@ added to itself @d@ times on the elliptic curve
  -- @c@. The parameter @n@ specifies the effective bit length of @d@, which
  -- may be greater than the actual bit length of @d@. The default
  -- implementation uses the Montgomery method of point multiplication to
  -- mitigate the leak of timing information.
  multiply :: (Field f) => EC c f -> Int -> Integer -> p c f -> p c f
  multiply c n d p = montgomery (add c) (negate c) (zeroPoint c) n p d




