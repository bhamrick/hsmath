module Math.Polynomial.Operations where

import Data.List
import Test.QuickCheck

_addP :: Num a => [a] -> [a] -> [a]
_addP as [] = as
_addP [] bs = bs
_addP (a:as) (b:bs) = (a + b) : _addP as bs

_subP :: Num a => [a] -> [a] -> [a]
_subP as [] = as
_subP [] bs = map negate bs
_subP (a:as) (b:bs) = (a - b) : _subP as bs

_negateP :: Num a => [a] -> [a]
_negateP = map negate

_naiveMultStep :: (Eq a, Num a) => ([a] -> [a] -> [a]) -> [a] -> [a] -> [a]
_naiveMultStep _ _ [] = []
_naiveMultStep _ [] _ = []
_naiveMultStep _ [a] [b] = [a*b]
_naiveMultStep mult p@(a:as) q@(b:bs) =
    _simplify $ a*b : map (*a) bs `_addP` map (*b) as `_addP` (0: mult as bs)

_naiveMultP :: (Eq a, Num a) => [a] -> [a] -> [a]
_naiveMultP = _naiveMultStep _naiveMultP

_shift :: Num a => Int -> [a] -> [a]
_shift _ [] = []
_shift 0 as = as
_shift n as = 0 : _shift (n-1) as

_karatsubaStep :: (Eq a, Num a) => ([a] -> [a] -> [a]) -> [a] -> [a] -> [a]
_karatsubaStep _ _ [] = []
_karatsubaStep _ [] _ = []
_karatsubaStep _ [a] [b] = [a*b]
_karatsubaStep mult as bs =
    let (s, t) = _split as
        (u, v) = _split bs
        x = intersperse 0 $ mult s u
        y = intersperse 0 $ mult t v
        z = intersperse 0 $ mult (_addP s t) (_addP u v)
    in _simplify $ x `_addP` _shift 1 (z `_subP` (x `_addP` y)) `_addP` _shift 2 y

_karatsubaP :: (Eq a, Num a) => [a] -> [a] -> [a]
_karatsubaP = _karatsubaStep _karatsubaP

_hybridMultStep :: (Eq a, Num a) => ([a] -> [a] -> [a]) -> [a] -> [a] -> [a]
_hybridMultStep mult as bs =
    let n = max (length as) (length bs) in
    if n < 10
    then _naiveMultStep mult as bs
    else _karatsubaStep mult as bs

_hybridMultP :: (Eq a, Num a) => [a] -> [a] -> [a]
_hybridMultP = _hybridMultStep _hybridMultP

_split :: [a] -> ([a], [a])
_split [] = ([], [])
_split (x:[]) = ([x], [])
_split (x:x':xs) = let (evens, odds) = _split xs in (x:evens, x':odds)

_evalP :: Num a => [a] -> a -> a
_evalP [] _ = 0
_evalP (a:as) x = a + x * _evalP as x

_simplify :: (Eq a, Num a) => [a] -> [a]
_simplify = reverse . dropWhile (== 0) . reverse

propAdd :: [Integer] -> [Integer] -> Integer -> Bool
propAdd p q x = _evalP (_addP p q) x == _evalP p x + _evalP q x

propSub :: [Integer] -> [Integer] -> Integer -> Bool
propSub p q x = _evalP (_subP p q) x == _evalP p x - _evalP q x

propNegate :: [Integer] -> Integer -> Bool
propNegate p x = _evalP (_negateP p) x == negate (_evalP p x)

propNaiveMult :: [Integer] -> [Integer] -> Integer -> Bool
propNaiveMult p q x = _evalP (_naiveMultP p q) x == _evalP p x * _evalP q x

propKaratsubaMult :: [Integer] -> [Integer] -> Integer -> Bool
propKaratsubaMult p q x = _evalP (_karatsubaP p q) x == _evalP p x * _evalP q x
