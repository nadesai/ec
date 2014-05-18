------------------------------------------------------------------------------
-- |
-- Module      : Crypto.EllipticCurve.Type
-- Copyright   : Brandon Azad and Nikhil Desai
-- License     : Public Domain
-- Stability   : experimental
--
-- The definitions of basic elliptic curve types.
--
------------------------------------------------------------------------------

module Crypto.EllipticCurve.Type
  (
    EC(..)
  ) where

import Crypto.Number.Field



-- | The @EC@ type holds an elliptic curve definition and the parameterization
-- of the underlying field.
data EC c f = EC (c f) (FieldOperations f)


