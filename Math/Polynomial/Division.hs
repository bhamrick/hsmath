module Math.Polynomial.Division
    ( divModPoly
    , monicDivMod
    , monicDiv
    , monicMod
    , squareFreePoly
    ) where

import Math.Modular
import Math.Polynomial.Types
import Math.Polynomial.Operations

divModPoly :: (Eq a, Fractional a) => P a -> P a -> (P a, P a)
divModPoly (P as) (P bs) =
    let (q', r') = _divModPoly (reverse as) (reverse bs)
    in (P (reverse . dropWhile (== 0) $ q'), P (reverse . dropWhile (== 0) $ r'))

instance (Eq a, Fractional a) => Euclidean (P a) where
    eDivMod = divModPoly

-- Note that here the coefficients are in most-significant first order
_divModPoly :: Fractional a => [a] -> [a] -> ([a], [a])
_divModPoly [] _ = ([], [])
_divModPoly _ [] = error "Division by zero polynomial"
_divModPoly p@(a:as) d@(b:bs) =
    if length p < length d
    then ([], p)
    else let c0 = a / b
             p' = _subPrefix as (map (* c0) bs)
             (q', r) = _divModPoly p' d
         in (c0:q', r)

_subPrefix :: Num a => [a] -> [a] -> [a]
_subPrefix [] _ = []
_subPrefix as [] = as
_subPrefix (a:as) (b:bs) = (a - b) : _subPrefix as bs

propQuotient :: P Rational -> P Rational -> Bool
propQuotient p q =
    q == P [] || let (x, y) = divModPoly p q in p == q * x + y

propEvaluate :: P Rational -> Rational -> Bool
propEvaluate p x =
    let d = P [-x, 1]
        (q, r) = divModPoly p d
    in r == P [evaluate p x]

-- |If the divisor is not monic, then this acts as division by the polynomial
-- where the leading coefficient has been replaced by 1.
monicDivMod :: Num a => P a -> P a -> (P a, P a)
monicDivMod (P as) (P bs) = let (q', r') = _monicDivMod (reverse as) (drop 1 $ reverse bs) in (P (reverse q'), P (reverse r'))

-- Here the coefficients are in most-significant first order and the divisor
-- has an implicit 1 at the beginning
_monicDivMod :: Num a => [a] -> [a] -> ([a], [a])
_monicDivMod [] _ = ([], [])
_monicDivMod p@(a:as) d@(bs) =
    if length p <= length d
    then ([], p)
    else let p' = _subPrefix as (map (* a) bs)
             (q', r) = _monicDivMod p' d
         in (a:q', r)

monicDiv :: Num a => P a -> P a -> P a
monicDiv p q = fst (monicDivMod p q)

monicMod :: Num a => P a -> P a -> P a
monicMod p q = snd (monicDivMod p q)

squareFreePoly :: (Eq a, Fractional a) => P a -> P a
squareFreePoly f = let g = eGcd f (derivative f) in eDiv f g
