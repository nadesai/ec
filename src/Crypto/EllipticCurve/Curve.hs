{-# LANGUAGE ExistentialQuantification #-}
------------------------------------------------------------------------------
-- |
-- Module      : Crypto.EllipticCurve.Curve
-- Copyright   : Brandon Azad and Nikhil Desai
-- License     : Public Domain
-- Stability   : experimental
--
-- Definitions of elliptic curves.
--
------------------------------------------------------------------------------

module Crypto.EllipticCurve.Curve
  (
    EC(..)
  ) where

import qualified Crypto.Number.Field as F
import Crypto.EllipticCurve.Group



data EC c f = forall p. (F.Field f, EllipticCurve c p) =>
              EC (c f) (F.FieldParameter f)




