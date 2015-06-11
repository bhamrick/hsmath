module Math.Modular where

import Data.Ratio
import GHC.Real

class Num a => Euclidean a where
    eDivMod :: a -> a -> (a, a)
    eDiv :: a -> a -> a
    eMod :: a -> a -> a

    eDiv x y = fst (eDivMod x y)
    eMod x y = snd (eDivMod x y)
    eDivMod x y = (eDiv x y, eMod x y)

instance Euclidean Word where
    eDivMod = divMod

instance Euclidean Int where
    eDivMod = divMod

instance Euclidean Integer where
    eDivMod = divMod

modPow :: Euclidean a => a -> Integer -> a -> a
modPow _ 0 _ = 1
modPow a e n =
    let x = modPow a (e `eDiv` 2) n
    in
    if even e
    then (x * x) `eMod` n
    else (a * x * x) `eMod` n

invMod :: (Eq a, Euclidean a) => a -> a -> Maybe a
invMod a n =
    let (x, y) = bezout a n
        g = a * x + n * y in
    if g /= 0 && 1 `eMod` g == 0
    then Just (x `eMod` n)
    else Nothing

-- bezout a b = (x,y) where a*x + b*y = gcd(a, b)
bezout :: (Eq a, Euclidean a) => a -> a -> (a, a)
bezout _ 0 = (1, 0)
bezout a b =
    -- r = a - q * b
    -- x' * b + y' * r = gcd(a, b)
    -- x' * b + y' * (a - q * b) = gcd(a, b)
    -- y' * a + (x' - q * y') b = gcd(a, b)
    let (q, r) = a `eDivMod` b in
    if r == 0
    then (0, 1)
    else let (x', y') = bezout b r in (y', x' - q * y')

modForm :: (Eq a, Euclidean a) => Ratio a -> a -> Maybe a
modForm (a :% b) n = fmap (\x -> (a * x) `eMod` n) (invMod b n)
