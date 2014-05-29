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
import Crypto.Number.Power ( expsq, expsq' )



-- | The Field class provides an interface for defining the algebraic
-- operations over a field, which may be parameterized by additional data.
class (Eq f) => Field f where

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
  rep p = flip $ expsq' (add p) (neg p) (zero)

  -- | Multiply two elements of the field.
  mul :: FieldParameter f -> f -> f -> f

  -- | Square an element of the field. This may be more efficient than
  -- multiplying an element by itself.
  sqr :: FieldParameter f -> f -> f
  sqr p a = mul p a a

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
  pow p = expsq (mul p) (sqr p) (inv p) (one)


-- | A record containing all the field operations specialized on a field
-- parameter. The @FieldOperations@ record is designed to be used together
-- with the @RecordWildCards@ language extension to quickly bring in familiar
-- names for field operations into scope. Note that this causes the existing
-- names of arithmetic operations to be shadowed and loses fixity.
data FieldOperations f = FieldOperations
  { (+) :: f -> f -> f        -- ^ field addition
  , (.-) :: f -> f            -- ^ field negation
  , (-) :: f -> f -> f        -- ^ field subtraction
  , (#) :: Integer -> f -> f  -- ^ repeated addition (multiplication by an
                              -- integer)
  , (*) :: f -> f -> f        -- ^ field multiplication
  , (^.) :: f -> f            -- ^ field squaring
  , (./) :: f -> f            -- ^ field inversion
  , (/) :: f -> f -> f        -- ^ field division
  , (^) :: f -> Integer -> f  -- ^ repeated multiplication (exponentiation by
                              -- an integer)
  }


-- | Get the field operations for the given field parameter.
ops :: (Field f) => FieldParameter f -> FieldOperations f
ops p = FieldOperations (add p) (neg p) (sub p) (rep p)
                        (mul p) (sqr p) (inv p) (div p) (pow p)



