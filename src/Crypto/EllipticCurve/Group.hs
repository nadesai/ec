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
import Crypto.EllipticCurve.Type
import Crypto.EllipticCurve.Point



-- | The class @EllipticCurve c p@ represents a formulation of a group law of
-- the curve @c@ over a point representation @p@. While all elliptic curves
-- @c@ should implement the same group law, the parameter @p@ allows different
-- representations of the curve to have different implementations.
class (EllipticCurvePoint c p) => EllipticCurve c p where

  add    :: (Field f) => EC c f -> p c f -> p c f -> p c f

  negate :: (Field f) => EC c f -> p c f -> p c f

  double :: (Field f) => EC c f -> p c f -> p c f




