{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
------------------------------------------------------------------------------
-- |
-- Module      : Crypto.Number.Mod
-- Copyright   : Brandon Azad
-- License     : Public Domain
-- Stability   : experimental
--
-- A @Crypto.Number.Field@ instance for @Data.Modular@.
--
------------------------------------------------------------------------------

module Crypto.Number.Mod
  (
    module Data.Modular
  ) where

import Data.Modular
import Data.Proxy   ( Proxy(..) )
import GHC.TypeLits ( KnownNat, natVal )

import Crypto.Number.Field hiding ( (+), (-), (*), (/), (^) )
import Crypto.Number.Modular   ( invm' )



instance (KnownNat n) => Field (Integer `Mod` n) where

  type FieldParameter (Integer `Mod` n) = ()

  zero = 0
  one = 1
  add _ = (+)
  neg _ = negate
  sub _ = (-)
  rep _ n a = fromInteger n * a
  mul _ = (*)
  inv _ a = toMod $ invm' (natVal (Proxy :: Proxy n)) (unMod a)



