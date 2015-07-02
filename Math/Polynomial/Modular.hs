module Math.Polynomial.Modular where

-- Helper functions for dealing with polynomials modulo monic polynomials.
-- If you're working over a field, you can just use the functions in Math.Modular,
-- as Math.Polynomial.Division exposes a Euclidean instance.
import Math.Modular
import Math.Polynomial.Types
import Math.Polynomial.Division
import Math.Polynomial.Operations (_simplify)

import Debug.Trace

monicModPow :: (Eq a, Num a) => P a -> Integer -> P a -> P a
monicModPow _ 0 _ = 1
monicModPow p e q =
    let x = monicModPow p (e `div` 2) q
    in
    if even e
    then (x * x) `monicMod` q
    else (p * x * x) `monicMod` q

-- This name is kind of silly.
modularModPow :: P Integer -> Integer -> P Integer -> Integer -> P Integer
modularModPow _ 0 _ _ = 1
modularModPow p e q n =
    let x = modularModPow p (e `div` 2) q n
    in
    if even e
    then modularModPoly (x * x) q n
    else modularModPoly (p * x * x) q n

reducePoly :: P Integer -> Integer -> P Integer
reducePoly (P as) p = P (_simplify $ map (`mod` p) as)

-- TODO: Consider non-error ways to die
-- Will throw an error if it ever encounters a number it can't invert.
-- This means (in many cases) that a nontrivial factor of n was found,
-- which would be useful to expose at a higher level.
modularPolyGcd :: P Integer -> P Integer -> Integer -> P Integer
modularPolyGcd f 0 _ = f
modularPolyGcd f g n =
    let (q, r) = modularDivModPoly f g n
    in modularPolyGcd g r n

modularDivModPoly :: P Integer -> P Integer -> Integer -> (P Integer, P Integer)
modularDivModPoly (P as) (P bs) n =
    let (qs, rs) = _modularDivModPoly (map (`mod` n) (reverse as)) (dropWhile (== 0) . map (`mod` n) . reverse $ bs) n
    in (P (reverse . dropWhile (== 0) $ qs), P (reverse . dropWhile (== 0) $ rs))

-- Note that coefficients are most-significant first for this function
_modularDivModPoly :: [Integer] -> [Integer] -> Integer -> ([Integer], [Integer])
_modularDivModPoly [] _ _ = ([], [])
_modularDivModPoly _ [] _ = error "Divison by zero polynomial"
_modularDivModPoly p@(a:as) d@(b:bs) n =
    if length p < length d
    then ([], p)
    else case invMod b n of
        Nothing -> error "Uninvertible element"
        Just b' ->
            let c0 = (a * b') `mod` n
                p' = map (`mod` n) (_subPrefix as (map (* c0) bs))
                (q', r) = _modularDivModPoly p' d n
            in (c0:q', r)
    where
    _subPrefix :: Num a => [a] -> [a] -> [a]
    _subPrefix [] _ = []
    _subPrefix as [] = as
    _subPrefix (a:as) (b:bs) = (a - b) : _subPrefix as bs

modularDivPoly :: P Integer -> P Integer -> Integer -> P Integer
modularDivPoly p q n = fst (modularDivModPoly p q n)

modularModPoly :: P Integer -> P Integer -> Integer -> P Integer
modularModPoly p q n = snd (modularDivModPoly p q n)

modularEvaluate :: P Integer -> Integer -> Integer -> Integer
modularEvaluate (P []) n x = 0
modularEvaluate (P (a:as)) n x = (a + x * modularEvaluate (P as) n x) `mod` n
