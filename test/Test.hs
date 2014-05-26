{-# LANGUAGE TypeSynonymInstances #-}
------------------------------------------------------------------------------
-- |
-- Module      : Test
-- Copyright   : Brandon Azad and Nikhil Desai
-- License     : Public Domain
-- Stability   : experimental
--
-- A test suite for EC.
--
------------------------------------------------------------------------------

module Main where

import Test.Hspec
import Test.QuickCheck

import qualified Crypto.EllipticCurve as EC

import BasicECC


p521_p :: Integer
p521_p = 0x01FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF


p521_a :: Integer
p521_a = 0x01FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC


p521_b :: Integer
p521_b = 0x0051953EB9618E1C9A1F929A21A0B68540EEA2DA725B99B315F3B8B489918EF109E156193951EC7E937B1652C0BD3BB1BF073573DF883D2C34F1EF451FD46B503F00


p521_x :: Integer
p521_x = 0x00C6858E06B70404E9CD9E3ECB662395B4429C648139053FB521F828AF606B4D3DBAA14B5E77EFE75928FE1DC127A2FFA8DE3348B3C1856A429BF97E7E31C2E5BD66


p521_y :: Integer
p521_y = 0x011839296a789a3bc0045c8a5fb42c7d1bd998f54449579b446817afbd17273e662c97ee72995ef42640c550b9013fad0761353c7086a272c24088be94769fd16650


-- | NIST P-521.
p521 :: WeierstrassCurve
p521 = WeierstrassPrimeCurve
  { weierstrassP = p521_p
  , weierstrassA = p521_a
  , weierstrassB = p521_b
  }


-- | The generator for NIST P-521.
g_p521 :: ECPoint
g_p521 = point p521 p521_x p521_y


-- | A shortened multiplication routine for NIST P-521.
mul :: ECPoint -> Integer -> ECPoint
mul = multiply p521

-- | A shortened point extraction routine for NIST P-521.
pt :: ECPoint -> Maybe (Integer, Integer)
pt = pointXY p521


-- | A 'BasicECC' point comparison routine for NIST P-521.
instance Eq ECPoint where
  p == q = pt p == pt q


-- | Generate a point on NIST P-521 as the given power of the generator.
ptAt :: Integer -> ECPoint
ptAt = mul g_p521

-- | Construct a property given a function operating on elliptic curve points.
pt_prop :: (Testable t) => (ECPoint -> t) -> Property
pt_prop f = property $ (\r -> f (ptAt r))


test1_n :: Integer
test1_n = 3^328


test1_x :: Integer
test1_x = 5715790403709750053768616578268015509136875310450688272969751157431253584249149583569349770914435730442932207763838653438442671856407929182001471411618973237


test1_y :: Integer
test1_y = 6475848461946159819873862769776881792588141351146685255249978050785956252205289113989562364711536239710024010252029615066973484135005962765214493789854989407


main :: IO ()
main = hspec $ describe "Testing EC" $ do

  describe "basic ECC" $ do

    it "the point at infinity multiplies to 0" $ property $
      \n -> (pt $ mul pointAtInfinity n) `shouldBe` Nothing

    it "multiplying by 1 is the identity transformation" $ pt_prop $
      \p -> p == mul p 1

    it "adding a point to itself is doubling" $ pt_prop $
      \p -> add p521 p p == double p521 p

    it "multiplying a point by 2 is doubling" $ pt_prop $
      \p -> mul p 2 == double p521 p

    it "test 1: point multiplication is correct" $ do
      let p = ptAt test1_n
      (pt p) `shouldBe` (Just (test1_x, test1_y))






