module Math.Polynomial.Division (divModPoly) where

import Math.Modular
import Math.Polynomial.Types

divModPoly :: Fractional a => P a -> P a -> (P a, P a)
divModPoly (P as) (P bs) = let (q', r') = _divModPoly (reverse as) (reverse bs) in (P (reverse q'), P (reverse r'))

instance (Eq a, Fractional a) => Euclidean (P a) where
    eDivMod = divModPoly

-- Note that here the coefficients are in most-significant first order
_divModPoly :: Fractional a => [a] -> [a] -> ([a], [a])
_divModPoly [] _ = ([], [])
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
