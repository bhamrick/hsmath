module Math.Polynomial.Coppersmith where

import Data.Ratio
import Math.Lattice.LLL (lll)
import Math.Polynomial

-- TODO: Figure out what a good bound B is
smallRoots :: P Integer -> Integer -> [Integer]
smallRoots p n = undefined

reducedPolynomial :: P Integer -> Integer -> Integer -> Int -> P Rational
reducedPolynomial p n b h = evaluate (fmap constPoly (P (head reducedLatticeBasis))) (1 % b .* polyVar)
    where
    d = degree p
    polynomialBasis = [((1 % n) .* fmap fromInteger p)^j * polyVar^i | i <- [0..d - 1], j <- [0..h-1]]
    -- polynomialBasis = (1 % n) .* fmap fromInteger p : [polyVar^i | i <- [0..d-1]]
    latticeBasis = [coefficients $ evaluate (fmap constPoly q) (fromInteger b * polyVar) | q <- polynomialBasis]
    reducedLatticeBasis = lll latticeBasis
