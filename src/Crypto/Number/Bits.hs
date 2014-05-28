{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
------------------------------------------------------------------------------
-- |
-- Module      : Crypto.Number.Bits
-- Copyright   : Brandon Azad
-- License     : Public Domain
-- Stability   : experimental
-- Portability : non-portable
--
-- Bitwise manipulation of integers.
--
------------------------------------------------------------------------------

module Crypto.Number.Bits
  (

  -- * Integer functions
    module Data.Bits
  , bitLength
  , byteLength
  , expmInteger
  , invmInteger
  , isPrime

  -- * Foreign memory
  , Endian(..)
  , writeIntegerBytes'
  , writeIntegerBytes
  , readIntegerBytes

  ) where

import GHC.Integer.GMP.Internals ( exportIntegerToAddr, importIntegerFromAddr,
                                   powModInteger, recipModInteger,
                                   sizeInBaseInteger, testPrimeInteger )
import GHC.Integer.Logarithms    ( integerLog2# )
import GHC.Types                 ( IO(..), Int(..), Word(..) )
import GHC.Ptr                   ( Ptr(..), plusPtr )
import Foreign.C.Types           ( CInt(..), CSize(..) )

import Data.Bits



-- | @bitLength n@ returns the bit length of the 'Integer' n.
bitLength :: Integer -> Int
bitLength n = I# (integerLog2# n) + 1


-- | @byteLength n@ returns the number of bytes required to store the integer
-- data in base-256 encoding. If @n == 0@ then 0 is returned.
byteLength :: Integer -> Word
byteLength 0 = 0
byteLength n = W# (sizeInBaseInteger n 256#)


-- | @expmInteger m b e@ efficiently computes @b^e `mod` m@.
expmInteger :: Integer -> Integer -> Integer -> Integer
expmInteger m b e = powModInteger b e m


-- | @invmIntegerm a@ efficiently computes the unique least positive integer
-- @b@ such that @ab `mod` m = 1@, if such a number exists.
invmInteger :: Integer -> Integer -> Integer
invmInteger m a = recipModInteger a m


-- | The result of a primality test.
data Primality = Composite | ProbablyPrime | Prime
               deriving (Eq, Ord, Show)


-- | @isPrime k n@ returns whether @n@ is composite, probably prime (with
-- probability dependent on the security factor @k@), or prime.
isPrime :: Int -> Integer -> Primality
isPrime (I# k#) n = case testPrimeInteger n k# of
  0# -> Composite
  1# -> ProbablyPrime
  2# -> Prime
  x  -> error $ "testPrimeInteger returned " ++ show (I# x)


-- | Endianness.
data Endian = BigEndian | LittleEndian
            deriving (Eq, Ord, Show)


-- | Convert the 'Endian' value to @1@ for big endian and @-1@ for little
-- endian.
endianToInt :: Endian -> Int
endianToInt BigEndian    =  1
endianToInt LittleEndian = -1

-- | C's @memset@.
foreign import ccall unsafe "string.h memset"
  c_memset :: Ptr a -> CInt -> CSize -> IO ()


-- | @zero ptr size@ zeroes the @size@ bytes at the memory address referenced
-- by @ptr@.
zero :: Ptr a -> Word -> IO ()
zero p size = c_memset p 0 (fromIntegral size)


-- | @writeIntegerBytes' ptr endian int@ writes the base-256 encoding of the
-- 'Integer' @int@ to the memory address pointed to by @ptr@. @int@ is written
-- to memory with the most significant byte first if @endian@ is @bigEndian@
-- and with the least significant byte first if @endian@ is @littleEndian@.
-- The number of bytes written is returned.
writeIntegerBytes' :: Ptr a -> Endian -> Integer -> IO Word
writeIntegerBytes' (Ptr addr#) endian int = IO $ \s# ->
  case exportIntegerToAddr int addr# ord# s# of
    (# s'#, written# #) -> (# s'#, W# written# #)
  where
    !(I# ord#) = endianToInt endian


-- | @writeIntegerBytes ptr size endian int@ writes the base-256 encoding of
-- the 'Integer' @int@ to the memory address pointed to by @ptr@. @size@ is
-- the size of the memory region that should be filled. If @size@ is smaller
-- than the number of bytes needed to store @int@ in memory, then an error is
-- thrown. @int@ is written to memory with the most significant byte first if
-- @endian@ is @bigEndian@ and with the least significant byte first if
-- @endian@ is @littleEndian@. If @int@ occupies fewer than @size@ bytes in
-- memory, then the remaining bytes are zeroed. The bytes of @int@ are written
-- to the end of the memory region if @endian@ is @bigEndian@ and to the start
-- of the memory region if @endian@ is @littleEndian@.
writeIntegerBytes :: Ptr a -> Word -> Endian -> Integer -> IO ()
writeIntegerBytes ptr size endian int
  | intSize > size = error "integer too large to be written to memory"
  | otherwise = do
      _ <- writeIntegerBytes' intPtr endian int
      zero remainderPtr remainderSize
  where
    intSize = byteLength int
    remainderSize = size - intSize
    (intPtr, remainderPtr) = case endian of
      BigEndian    -> (plusPtr ptr (fromIntegral remainderSize), ptr)
      LittleEndian -> (ptr, plusPtr ptr (fromIntegral intSize))


-- | @readIntegerBytes ptr size endian@ reads the base-256 encoding of an
-- integer at the memory location referenced by @ptr@ and returns the
-- corresponding Haskell 'Integer'. If @endian@ is @bigEndian@ then the first
-- byte in memory becomes the most significant byte of the resulting integer,
-- and if @endian@ is @littleEndian@ then the first byte in memory becomes the
-- least significant byte of the resulting integer.
readIntegerBytes :: Ptr a -> Word -> Endian -> IO Integer
readIntegerBytes (Ptr addr#) (W# size#) endian = IO $ \s# ->
  importIntegerFromAddr addr# size# ord# s#
  where
    !(I# ord#) = endianToInt endian


