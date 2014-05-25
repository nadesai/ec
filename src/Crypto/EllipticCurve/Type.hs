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
  -- * Elliptic curve types.
    EC(..)
  , ECP(..)
  ) where

import Crypto.Number.Field



-- | The @EC@ type holds an elliptic curve definition and the parameterization
-- of the underlying field.
data EC c f = EC (c f) (FieldOperations f)


-- | The elliptic curve domain parameters for a cyclic subgroup of the
-- elliptic curve group.
data ECP c p f = ECP
  { ecpCurve     :: EC c f
  , ecpGenerator :: p c f
  , ecpOrder     :: Integer
  , ecpCofactor  :: Integer
  }

