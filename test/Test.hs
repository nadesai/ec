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

import Crypto.Number
import qualified Crypto.EllipticCurve as EC
import qualified Crypto.EllipticCurve.StandardCurves as S

import BasicECC



------------------------------------------------------------------------------
-- NIST P-521 definition.
------------------------------------------------------------------------------


p521_p :: (Num a) => a
p521_p = 0x01FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF


p521_a :: (Num a) => a
p521_a = 0x01FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC


p521_b :: (Num a) => a
p521_b = 0x0051953EB9618E1C9A1F929A21A0B68540EEA2DA725B99B315F3B8B489918EF109E156193951EC7E937B1652C0BD3BB1BF073573DF883D2C34F1EF451FD46B503F00


p521_x :: (Num a) => a
p521_x = 0x00C6858E06B70404E9CD9E3ECB662395B4429C648139053FB521F828AF606B4D3DBAA14B5E77EFE75928FE1DC127A2FFA8DE3348B3C1856A429BF97E7E31C2E5BD66


p521_y :: (Num a) => a
p521_y = 0x011839296a789a3bc0045c8a5fb42c7d1bd998f54449579b446817afbd17273e662c97ee72995ef42640c550b9013fad0761353c7086a272c24088be94769fd16650


------------------------------------------------------------------------------
-- Correctness test points.
------------------------------------------------------------------------------


test1_n :: Integer
test1_n = 3^328


test1_x :: Integer
test1_x = 5715790403709750053768616578268015509136875310450688272969751157431253584249149583569349770914435730442932207763838653438442671856407929182001471411618973237


test1_y :: Integer
test1_y = 6475848461946159819873862769776881792588141351146685255249978050785956252205289113989562364711536239710024010252029615066973484135005962765214493789854989407


------------------------------------------------------------------------------
-- BasicECC implementation.
------------------------------------------------------------------------------


-- | NIST P-521.
b_p521 :: WeierstrassCurve
b_p521 = WeierstrassPrimeCurve
  { weierstrassP = p521_p
  , weierstrassA = p521_a
  , weierstrassB = p521_b
  }


-- | The generator for NIST P-521.
b_gp521 :: ECPoint
b_gp521 = point b_p521 p521_x p521_y


-- | A shortened multiplication routine for NIST P-521.
b_mul :: ECPoint -> Integer -> ECPoint
b_mul = multiply b_p521


-- | A shortened addition routine for NIST P-521.
b_add :: ECPoint -> ECPoint -> ECPoint
b_add = add b_p521


-- | A shortened doubling routine for NIST P-521.
b_dbl :: ECPoint -> ECPoint
b_dbl = double b_p521


-- | A shortened point extraction routine for NIST P-521.
b_pt :: ECPoint -> Maybe (Integer, Integer)
b_pt = pointXY b_p521


-- | A 'BasicECC' point comparison routine for NIST P-521.
instance Eq ECPoint where
  p == q = b_pt p == b_pt q


-- | Generate a point on NIST P-521 as the given power of the generator.
b_ptAt :: Integer -> ECPoint
b_ptAt = b_mul b_gp521

-- | Construct a property given a function operating on elliptic curve points.
b_pt_prop :: (Testable t) => (ECPoint -> t) -> Property
b_pt_prop f = property $ (\r -> f (b_ptAt r))


------------------------------------------------------------------------------
-- BasicECC implementation.
------------------------------------------------------------------------------

type Point = EC.Affine EC.Weierstrass S.P521

e_p521 :: EC.EC EC.Weierstrass S.P521
e_p521 = S.p521

e_gp521 :: Point
e_gp521 = EC.ecpGenerator S.nistP521


-- | This is not the cleanest way to write point multiplication; a cleaner
-- interface should be defined over ECP.
e_mul :: Point -> Integer -> Point
e_mul p d = EC.multiply e_p521 521 d p


-- | Get the affine x- and y-coordinates.
e_pt :: Point -> Maybe (Integer, Integer)
e_pt p = do
  (x', y') <- EC.affineXY S.p521 p
  return (unMod x', unMod y')


-- | Returns the given multiple of the generator.
e_ptAt :: Integer -> Point
e_ptAt n = e_mul e_gp521 n


------------------------------------------------------------------------------
-- Tests.
------------------------------------------------------------------------------


main :: IO ()
main = hspec $ describe "Testing EC" $ do

  describe "BasicECC" $ do

    it "the point at infinity multiplies to 0" $ property $
      \n -> (b_pt $ b_mul pointAtInfinity n) `shouldBe` Nothing

    it "multiplying by 1 is the identity transformation" $ b_pt_prop $
      \p -> p == b_mul p 1

    it "adding a point to itself is doubling" $ b_pt_prop $
      \p -> b_add p p == b_dbl p

    it "multiplying a point by 2 is doubling" $ b_pt_prop $
      \p -> b_mul p 2 == b_dbl p

    it "multiplication is commutative" $ property $
      \m n -> b_mul (b_ptAt m) n == b_mul (b_ptAt n) m

    it "test 1: point multiplication is correct" $ do
      let p = b_ptAt test1_n
      (b_pt p) `shouldBe` (Just (test1_x, test1_y))

  describe "EC correctness" $ do

    it "EC and BasicECC agree on point multiplication" $ property $
      \n m -> b_pt (b_mul (b_ptAt n) m) == e_pt (e_mul (e_ptAt n) m)






