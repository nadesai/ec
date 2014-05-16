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
import qualified Crypto.Number.Field as F
import Crypto.EllipticCurve.Type
import Crypto.EllipticCurve.Point



-- | The class @EllipticCurve c p@ represents a formulation of a group law of
-- the curve @c@ over a point representation @p@. While all elliptic curves
-- @c@ should implement the same group law, the parameter @p@ allows different
-- representations of the curve to have different implementations.
class EllipticCurve c p where

  add :: (F.Field f) => EC c f -> p c f -> p c f -> p c f

  negate :: (F.Field f) => EC c f -> p c f -> p c f

  double :: (F.Field f) => EC c f -> p c f -> p c f




