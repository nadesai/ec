--
-- Test.hs
--
-- Brandon Azad
-- Nikhil Desai
--
-- Public Domain
--
--

-- |
-- A test suite for this implementation of ECC.
--


module Main where

import Test.Hspec
import Test.QuickCheck

import Crypto.EllipticCurve

import BasicECC



-- | NIST P-521.
p521 :: WeierstrassCurve
p521 = WeierstrassPrimeCurve
  { weierstrassP = 0x01FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
  , weierstrassA = 0x01FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC
  , weierstrassB = 0x0051953EB9618E1C9A1F929A21A0B68540EEA2DA725B99B315F3B8B489918EF109E156193951EC7E937B1652C0BD3BB1BF073573DF883D2C34F1EF451FD46B503F00
  }


-- | The generator for NIST P-521.
g_p521 :: ECPoint
g_p521 = point p521
  0x00C6858E06B70404E9CD9E3ECB662395B4429C648139053FB521F828AF606B4D3DBAA14B5E77EFE75928FE1DC127A2FFA8DE3348B3C1856A429BF97E7E31C2E5BD66
  0x011839296a789a3bc0045c8a5fb42c7d1bd998f54449579b446817afbd17273e662c97ee72995ef42640c550b9013fad0761353c7086a272c24088be94769fd16650




-- | A shortened multiplication routine for NIST P-521.
mul :: ECPoint -> Integer -> ECPoint
mul = multiply p521

-- | A shortened point extraction routine for NIST P-521.
pt :: ECPoint -> Maybe (Integer, Integer)
pt = pointXY p521

-- | A point comparison routine for NIST P-521.
(=.=) :: ECPoint -> ECPoint -> Bool
p =.= q = pt p == pt q

infix 4 =.=


-- | Generate a point on NIST P-521 as the given power of the generator.
ptAt :: Integer -> ECPoint
ptAt = mul g_p521

-- | Construct a property given a function operating on elliptic curve points.
pt_prop :: (Testable t) => (ECPoint -> t) -> Property
pt_prop f = property $ (\r -> f (ptAt r))


main :: IO ()
main = hspec $ describe "Testing Lab 3" $ do

  describe "basic ECC" $ do

    it "the point at infinity multiplies to 0" $ property $
      \n -> (pt $ mul pointAtInfinity n) `shouldBe` Nothing

    it "multiplying by 1 is the identity transformation" $ pt_prop $
      \p -> p =.= mul p 1

    it "adding a point to itself is doubling" $ pt_prop $
      \p -> add p521 p p =.= double p521 p

    it "multiplying a point by 2 is doubling" $ pt_prop $
      \p -> mul p 2 =.= double p521 p

    it "test 1: point multiplication is correct" $ do
      let p = ptAt (3^328)
      (pt p) `shouldBe` (Just (5715790403709750053768616578268015509136875310450688272969751157431253584249149583569349770914435730442932207763838653438442671856407929182001471411618973237,6475848461946159819873862769776881792588141351146685255249978050785956252205289113989562364711536239710024010252029615066973484135005962765214493789854989407))






