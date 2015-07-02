module Math.Polynomial.Coppersmith where

import Debug.Trace

import Control.Monad.Random
import Data.List
import Data.Maybe
import Data.Ratio
import Math.Lattice.LLL (lll)
import Math.Polynomial
import Math.Polynomial.Hensel
import Math.Polynomial.Modular
import Math.Polynomial.ModularRoots
import Math.Primes
import System.Random

smallModularRoots :: P Integer -> Integer -> [Integer]
smallModularRoots p n =
    let eps = 1 / (3 * fromIntegral (degree p))
        b = round (0.5 * exp ((1/(fromIntegral (degree p)) - eps) * log (fromInteger n)))
        h = ceiling (1 / (fromIntegral (degree p) * eps))
        candidates = smallModularRoots' p n b h
    in filter (\x -> modularEvaluate p n x == 0) candidates

smallModularRoots' :: P Integer -> Integer -> Integer -> Int -> [Integer]
smallModularRoots' p n b h =
    let r = reducedPolynomial p n b h
        candidates = smallRoots (integerMultiple . squareFreePoly $ r) b
    in filter (\x -> gcd (modularEvaluate p n x) n > 1) candidates

-- Given a semiprime N and the high bits of the larger factor of N, attempts to find the
-- remainder of the factor.
-- Since we're using a degree 1 polynomial, we need h >= max(1/4eps, 7/2).
-- Using h = 4, we have eps = 1/16, so we will find the factor when the size
-- of the unknown part is at most 1/2 * N^(1/4 - 1/16) = 1/2 * N^(3/16).
-- For simplicity, we'll have the bound on the remainder as an argument.
partialFactor :: Integer -> Integer -> Integer -> Maybe Integer
partialFactor n partialP b =
    let results = smallModularRoots' (P [partialP, 1]) n b 4
    in fmap ((+) partialP) . listToMaybe $ results

reducedPolynomial :: P Integer -> Integer -> Integer -> Int -> P Rational
reducedPolynomial p n b h = evaluate (fmap constPoly (P (head reducedLatticeBasis))) (1 % b .* polyVar)
    where
    d = degree p
    polynomialBasis = [((1 % n) .* fmap fromInteger p)^j * polyVar^i | i <- [0..d - 1], j <- [0..h-1]]
    -- polynomialBasis = (1 % n) .* fmap fromInteger p : [polyVar^i | i <- [0..d-1]]
    latticeBasis = [coefficients $ evaluate (fmap constPoly q) (fromInteger b * polyVar) | q <- polynomialBasis]
    reducedLatticeBasis = lll latticeBasis

-- |smallRoots f b computes the integer roots with magnitude at most b
smallRoots :: P Integer -> Integer -> [Integer]
smallRoots f b = sort $ filter (\x -> evaluate f x == 0) rootCandidates
    where
    p :: Integer
    p = evalRand (findUsablePrime 2) (mkStdGen 1)
    f' :: P Integer
    f' = integerMultiple . squareFreePoly . fmap fromInteger $ f
    findUsablePrime :: MonadRandom m => Int -> m Integer
    findUsablePrime k = do
        candidate <- randomPrime k
        let g = modularPolyGcd (reducePoly f' candidate) (reducePoly (derivative f') candidate) candidate
        if degree g == 1
            then return candidate
            else findUsablePrime (k+1)
    liftResidue :: Integer -> [Integer]
    liftResidue = liftResidue' 1
    liftResidue' :: Int -> Integer -> [Integer]
    liftResidue' r x = if p^r > b
        then [x, x - p^r]
        else maybeToList (henselLift f' x p r) >>= liftResidue' (2*r)
    rootCandidates :: [Integer]
    rootCandidates = rootsModP f p >>= liftResidue
