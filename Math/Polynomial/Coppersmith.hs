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

-- TODO: Figure out what a good bound B is
smallModularRoots :: P Integer -> Integer -> [Integer]
smallModularRoots p n = undefined

smallModularRoots' :: P Integer -> Integer -> Integer -> Int -> [Integer]
smallModularRoots' p n b h =
    let r = reducedPolynomial p n b h
        candidates = smallRoots (integerMultiple . squareFreePoly $ r) b
    in filter (\x -> modularEvaluate p n x == 0) candidates

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
