{-# LANGUAGE ExistentialQuantification #-}
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

import qualified Crypto.Number.Field as F



-- | The @EC@ type holds an elliptic curve definition and the parameterization
-- of the underlying field.
data EC c f = EC (c f) (F.FieldParameter f)


