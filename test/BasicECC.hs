--
-- BasicECC.hs
--
-- Module BasicECC
--
-- Brandon Azad
-- Nikhil Desai
--
-- Public Domain
--
--

-- |
-- A test module to implement ECC on Weierstrass curves over prime fields.
--

module BasicECC
( WeierstrassCurve(..)
, ECPoint
, point
, pointAtInfinity
, pointXY
, onCurve
, multiply
, add
, double
) where



import Crypto.Number ( invm', bitLength )
import Data.Bits     ( testBit )



-- | A Weierstrass form elliptic curve. Currently only curves over prime order
-- fields are supported.
data WeierstrassCurve = WeierstrassPrimeCurve
  { weierstrassA :: Integer
  , weierstrassB :: Integer
  , weierstrassP :: Integer
  } deriving (Show)


-- | A point on an elliptic curve in affine coordinates.
data AffinePoint = AffinePoint
  { affineX :: Integer
  , affineY :: Integer
  } deriving (Show)


-- | A point on an elliptic curve in Jacobian coordinates.
-- The Jacobian point (X,Y,Z) represents the affine point (X/Z^2,Y/Z^3).
data JacobianPoint = JacobianPoint
  { jacobianX :: Integer
  , jacobianY :: Integer
  , jacobianZ :: Integer
  } deriving (Show)


-- | An internal representation of an elliptic curve point.
type ECPoint = JacobianPoint


-- | Construct an affine point on the given elliptic curve.
affine :: WeierstrassCurve -> Integer -> Integer -> AffinePoint
affine (WeierstrassPrimeCurve { weierstrassP = p }) x y =
  AffinePoint (x `mod` p) (y `mod` p)


-- | Convert a point from affine coordinates to some more computationally
-- efficient coordinate system.
fromAffine :: AffinePoint -> JacobianPoint
fromAffine (AffinePoint { affineX = x, affineY = y }) = JacobianPoint x y 1


-- | The inverse of 'fromAffine'. If the point represents the point at infinity
-- on the elliptic curve group, then Nothing is returned; otherwise, the point
-- is converted to affine coordinates and returned.
toAffine :: WeierstrassCurve -> JacobianPoint -> Maybe AffinePoint
toAffine (WeierstrassPrimeCurve { weierstrassP = p })
         (JacobianPoint { jacobianX = x, jacobianY = y, jacobianZ = z })
  | z == 0    = Nothing
  | otherwise = Just $ let z3    = (z * z `mod` p) * z `mod` p
                           z3inv = invm' p z3
                           z2inv = z3inv * z `mod` p
                       in AffinePoint (x * z2inv `mod` p) (y * z3inv `mod` p)


-- | Construct a point in an efficient coordinate system on the given curve
-- with the given x and y coordinates.
point :: WeierstrassCurve -> Integer -> Integer -> JacobianPoint
point c x y = fromAffine $ affine c x y


-- | Construct the point at infinity.
pointAtInfinity :: JacobianPoint
pointAtInfinity = JacobianPoint 1 1 0


-- | Converts an internal representation of a point into its x and y
-- coordinates, if the point does not represent the point at infinity on the
-- curve.
pointXY :: WeierstrassCurve -> JacobianPoint -> Maybe (Integer, Integer)
pointXY c p = do
  AffinePoint x y <- toAffine c p
  return (x,y)


-- | Returns True if and only if the point represents the unique point at
-- infinity on the elliptic curve.
isPointAtInfinity :: JacobianPoint -> Bool
isPointAtInfinity (JacobianPoint { jacobianZ = z }) = z == 0


-- | Returns True if and only if the point is on the given curve.
onCurve :: WeierstrassCurve -> JacobianPoint -> Bool
onCurve _ pt | isPointAtInfinity pt = True
onCurve (WeierstrassPrimeCurve { weierstrassP = p, weierstrassA = a
                               , weierstrassB = b })
        (JacobianPoint { jacobianX = x, jacobianY = y, jacobianZ = z })
  = let c *% d = c * d `mod` p
        c +% d = (c + d) `mod` p
        z2 = z *% z
        z4 = z2 *% z2
        z6 = z2 *% z4
    in y *% y == (x *% x *% x) +% (a *% x *% z4) +% (b *% z6)


-- | Add elliptic curve points.
-- hyperelliptic.org/EFD/g1p/auto-shortw-jacobian.html
add :: WeierstrassCurve -> JacobianPoint -> JacobianPoint -> JacobianPoint
add c@(WeierstrassPrimeCurve { weierstrassP = p })
     p0@(JacobianPoint { jacobianX = xP, jacobianY = yP, jacobianZ = zP })
     q0@(JacobianPoint { jacobianX = xQ, jacobianY = yQ, jacobianZ = zQ })
  | zP == 0   = q0
  | zQ == 0   = p0
  | h == 0    = if r == 0
                  then double c p0            -- TODO: share zP2
                  else pointAtInfinity
  | otherwise = JacobianPoint xR yR zR        -- 11M, 5S, 4(*2), 9A
  where
    zP2 = zP^2 `mod` p                        -- 1S
    zQ2 = zQ^2 `mod` p                        -- 1S
    u1  = xP * zQ2 `mod` p                    -- 1M
    u2  = xQ * zP2 `mod` p                    -- 1M
    s1  = (yP * zQ `mod` p) * zQ2 `mod` p     -- 2M
    s2  = (yQ * zP `mod` p) * zP2 `mod` p     -- 2M
    h   = (u2 - u1) `mod` p                   -- 1A
    i   = (2 * h)^2 `mod` p                   -- 1S, 1(*2)
    j   = h * i `mod` p                       -- 1M
    r   = 2 * (s2 - s1) `mod` p               -- 1(*2), 1A
    v   = u1 * i `mod` p                      -- 1M
    xR  = (r^2 - j - 2 * v) `mod` p           -- 1S, 1(*2), 2A
    yR  = (r * (v - xR) - 2 * s1 * j) `mod` p -- 2M, 1(*2), 2A
    zR  = (((zP + zQ)^2 - zP2 - zQ2) `mod` p) * h `mod` p
                                              -- 1M, 1S, 3A


-- | Double elliptic curve points.
-- hyperelliptic.org/EFD/g1p/auto-shortw-jacobian.html
double :: WeierstrassCurve -> JacobianPoint -> JacobianPoint
double (WeierstrassPrimeCurve { weierstrassP = p, weierstrassA = a })
        (JacobianPoint { jacobianX = xP, jacobianY = yP, jacobianZ = zP })
  = let xP2 = xP^2 `mod` p                        -- 1S
        yP2 = yP^2 `mod` p                        -- 1S
        yP4 = yP2^2 `mod` p                       -- 1S
        zP2 = zP^2 `mod` p                        -- 1S
        s   = 2 * ((xP + yP2)^2 - xP2 - yP4) `mod` p
                                                  -- 1S, 1(*2), 3A
        m   = (3 * xP2 + a * zP2^2) `mod` p
                                                  -- 1S, 1(*a), 1(*3), 1A
        xR  = (m^2 - 2 * s) `mod` p               -- 1S, 1(*2), 1A
        yR  = (m * (s - xR) - 8 * yP4) `mod` p    -- 1M, 1(*8), 2A
        zR  = ((yP + zP)^2 - yP2 - zP2) `mod` p
                                                  -- 1S, 3A
    in JacobianPoint xR yR zR                     -- 1M, 8S, 1(*a), 1(*8),
                                                  -- 1(*3), 2(*2), 10A



-- | Multiply the given point on the elliptic curve by the given scalar value.
-- This operation is equivalent to adding the point to itself under the group
-- operation the specified number of times.
-- This implementation is based on the Montgomery ladder, which is designed to
-- be secure against timing attacks.
-- However, information may still be leaked.
multiply :: WeierstrassCurve -> JacobianPoint -> Integer -> JacobianPoint
multiply c p0 d = go pointAtInfinity p0 n0
  where
    n0 = case c of
      WeierstrassPrimeCurve { weierstrassP = p } -> bitLength p
    go r0 r1 n
      | n == -1     = r0 `seq` r1 `seq` r0
      | testBit d n = go (add c r0 r1) (double c r1) (n-1)
      | otherwise   = go (double c r0) (add c r0 r1) (n-1)



