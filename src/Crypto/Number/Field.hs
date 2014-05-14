{-# LANGUAGE TypeFamilies #-}
------------------------------------------------------------------------------
-- |
-- Module      : Crypto.Number.Field
-- Copyright   : Brandon Azad and Nikhil Desai
-- License     : Public Domain
-- Stability   : experimental
--
-- Definition of algebraic fields.
--
------------------------------------------------------------------------------

module Crypto.Number.Field
  (

  -- * Field definition

    Field(..)

  -- * Accessors

  , FieldOperations(..)
  , ops

  ) where



-- | The Field class provides an interface for defining the algebraic
-- operations over a field, which may be parameterized by additional data.
class Field f where

  -- | The field parameterization type. This parameter allows the field to be
  -- dynamically parameterized by runtime data. If the field type does not
  -- need to be parameterized, use @()@.
  type FieldParameter f

  add :: FieldParameter f -> f -> f -> f

  neg :: FieldParameter f -> f -> f

  rep :: FieldParameter f -> f -> Integer -> f

  mul :: FieldParameter f -> f -> f -> f

  inv :: FieldParameter f -> f -> f

  pow :: FieldParameter f -> f -> Integer -> f


-- | A record containing all the field operations specialized on a field
-- parameter.
data FieldOperations f = FieldOperations
  { fadd :: f -> f -> f
  , fneg :: f -> f
  , frep :: f -> Integer -> f
  , fmul :: f -> f -> f
  , finv :: f -> f
  , fpow :: f -> Integer -> f
  }


-- | Get the field operations for the given field parameter.
ops :: (Field f) => FieldParameter f -> FieldOperations f
ops p = FieldOperations (add p) (neg p) (rep p) (mul p) (inv p) (pow p)


