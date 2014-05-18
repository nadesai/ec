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

import Prelude hiding      ( div )
import Crypto.Number.Power ( expsq )



-- | The Field class provides an interface for defining the algebraic
-- operations over a field, which may be parameterized by additional data.
class Field f where

  -- | The field parameterization type. This parameter allows the field to be
  -- dynamically parameterized by runtime data. If the field type does not
  -- need to be parameterized, use @()@.
  type FieldParameter f

  -- | The zero element of the field.
  zero :: f

  -- | The one element of the field.
  one :: f

  -- | Add two elements of the field.
  add :: FieldParameter f -> f -> f -> f

  -- | Negate an element of the field.
  neg :: FieldParameter f -> f -> f

  -- | Subtract the second field element from the first field element.
  -- @sub p a b = 'add' p a ('neg' p b)@.
  sub :: (Field f) => FieldParameter f -> f -> f -> f
  sub p a b = add p a (neg p b)

  -- | Multiply a field element by an integer. This is equivalent to
  -- repeatedly adding the field element to itself.
  rep :: FieldParameter f -> Integer -> f -> f
  rep p = flip $ expsq (add p) (neg p) (zero)

  -- | Multiply two elements of the field.
  mul :: FieldParameter f -> f -> f -> f

  -- | Invert a nonzero element of the field.
  inv :: FieldParameter f -> f -> f

  -- | Divide the first field element by the second (nonzero) field element.
  -- @div p a b = 'mul' p a ('inv' p b)@.
  div :: (Field f) => FieldParameter f -> f -> f -> f
  div p a b = mul p a (inv p b)

  -- | Raise an element of the field to the given integral power. When the
  -- exponent is positive, this is equivalent to repeatedly multiplying the
  -- field element by itself.
  pow :: FieldParameter f -> f -> Integer -> f
  pow p = expsq (mul p) (inv p) (one)


-- | A record containing all the field operations specialized on a field
-- parameter.
data FieldOperations f = FieldOperations
  { fadd :: f -> f -> f
  , fneg :: f -> f
  , fsub :: f -> f -> f
  , frep :: Integer -> f -> f
  , fmul :: f -> f -> f
  , finv :: f -> f
  , fdiv :: f -> f -> f
  , fpow :: f -> Integer -> f
  }


-- | Get the field operations for the given field parameter.
ops :: (Field f) => FieldParameter f -> FieldOperations f
ops p = FieldOperations (add p) (neg p) (sub p) (rep p)
                        (mul p) (inv p) (div p) (pow p)



