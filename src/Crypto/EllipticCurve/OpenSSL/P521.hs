{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedFFITypes #-}
------------------------------------------------------------------------------
-- |
-- Module      : Crypto.EllipticCurve.OpenSSL.P521
-- Copyright   : Brandon Azad and Nikhil Desai
-- License     : Public Domain
-- Stability   : experimental
-- Portability : non-portable
--
-- The prime field modulo @2^521 - 1@ from OpenSSL.
--
------------------------------------------------------------------------------

module Crypto.EllipticCurve.OpenSSL.P521
  (
    P521
  , getP521
  , fromInteger
  , toInteger
  , neg
  , add
  , sub
  , mul
  , sqr
  ) where


import Prelude hiding ( fromInteger, toInteger )
import GHC.ForeignPtr
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc ( allocaBytes )
import Foreign.Ptr
import System.IO.Unsafe

import Crypto.Number.Bits


-- | A type representing the data backing a P521 field element. @P521Data@ is
-- only ever used at the type level to distinguish pointers to different data
-- types.
data P521Data


-- | An element in the field of integers modulo @2^521 - 1@.
newtype P521 = P521 (ForeignPtr P521Data)


-- | Showing a 'P521' field element converts the element to an integer and
-- then shows that integer.
instance Show P521 where
  show p = show (toInteger p)


-- | Extract a 'ForeignPtr' to the data backing the 'P521' field element.
getP521 :: P521 -> ForeignPtr P521Data
getP521 (P521 ptr) = ptr


-- | A 'Felem' stores 72 bytes of data.
felemSize :: Int
felemSize = 72


{-
-- | A 'LargeFelem' stores 144 bytes of data.
largeFelemSize :: Int
largeFelemSize = 144
-}


-- | When converting a 'Felem' to its binary representation as an integer, the
-- intermediate buffer storing the raw integer memory needs to be 66 bytes
-- wide.
binSize :: Int
binSize = 66


-- | A @Felem@ is a pointer to memory storing 72 bytes representing the P521
-- field element.
type Felem = Ptr P521Data


-- | A type annotation representing the fact that the given argument to a C
-- function is a destination.
type Dst a = a


-- | Allocate the specified amount of memory on the Haskell heap.
mallocBytes :: Int -> IO (ForeignPtr a)
mallocBytes = mallocPlainForeignPtrBytes


-- | Create a new uninitialized 'P521' object.
newP521 :: IO P521
newP521 = P521 `fmap` mallocBytes felemSize


foreign import ccall unsafe "p521.h felem_neg"
  c_felem_neg :: Dst Felem -> Felem -> IO ()


foreign import ccall unsafe "p521.h felem_sum"
  c_felem_sum :: Dst Felem -> Ptr a -> Ptr a -> IO ()


foreign import ccall unsafe "p521.h felem_diff"
  c_felem_diff :: Dst Felem -> Ptr a -> Ptr a -> IO ()


foreign import ccall unsafe "p521.h felem_mul_reduce"
  c_felem_mul :: Dst Felem -> Ptr a -> Ptr a -> IO ()


foreign import ccall unsafe "p521.h felem_square_reduce"
  c_felem_sqr :: Dst Felem -> Ptr a -> IO ()


foreign import ccall unsafe "p521.h bin66_to_felem"
  c_bin66_to_felem :: Dst Felem -> Ptr a -> IO ()


foreign import ccall unsafe "p521.h felem_to_bin66"
  c_felem_to_bin66 :: Dst Felem -> Ptr a -> IO ()


-- | 'make fun' constructs a 'P521' using the given destructive function.
make :: (Dst Felem -> IO ()) -> IO P521
make fun = do
  (P521 p) <- newP521
  withForeignPtr p $ \dst -> fun dst
  return $ P521 p


-- | 'call1 fun' converts the given destructive function into a Haskell
-- function that creates a 'P521' field element from a single source element.
call1 :: (Dst Felem -> Felem -> IO ()) -> P521 -> P521
call1 fun (P521 p) = unsafePerformIO $ do
  withForeignPtr p $ \felem -> do
    make (\dst -> fun dst felem)


-- | 'call2 fun' converts the given destructive function into a Haskell
-- function that creates a 'P521' field element from two source elements.
call2 :: (Dst Felem -> Felem -> Felem -> IO ()) -> P521 -> P521 -> P521
call2 fun (P521 p1) (P521 p2) = unsafePerformIO $ do
  withForeignPtr p1 $ \felem1 -> do
    withForeignPtr p2 $ \felem2 -> do
      make $ \dst -> fun dst felem1 felem2


-- | Negate a 'P521' field element.
neg :: P521 -> P521
neg = call1 c_felem_neg


-- | Add two 'P521' field elements.
add :: P521 -> P521 -> P521
add = call2 c_felem_sum


-- | Add two 'P521' field elements.
sub :: P521 -> P521 -> P521
sub = call2 c_felem_diff


-- | Add two 'P521' field elements.
mul :: P521 -> P521 -> P521
mul = call2 c_felem_mul


-- | Add two 'P521' field elements.
sqr :: P521 -> P521
sqr = call1 c_felem_sqr


-- | Convert an integer into an element of the field 'P521'.
fromInteger :: Integer -> P521
fromInteger i | i < 0 =
  error "cannot convert a negative integer to a field element"
fromInteger i | bitLength i > 521 =
  error "integer too large to convert to a field element"
fromInteger i = unsafePerformIO $ do
  allocaBytes binSize $ \buf -> do
    writeIntegerBytes buf (fromIntegral binSize) LittleEndian i
    make (\dst -> c_bin66_to_felem dst buf)


-- | Convert the given field element to an 'Integer'.
toInteger :: P521 -> Integer
toInteger (P521 p) = unsafePerformIO $ do
  allocaBytes binSize $ \buf -> do
    withForeignPtr p $ \felem -> c_felem_to_bin66 buf felem
    readIntegerBytes buf (fromIntegral binSize) LittleEndian





