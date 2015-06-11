module Math.Modular where

import Data.Ratio
import GHC.Real

modPow :: Integral a => a -> Integer -> a -> a
modPow _ 0 _ = 1
modPow a e n =
    let x = modPow a (e `div` 2) n
    in
    if even e
    then (x * x) `mod` n
    else (a * x * x) `mod` n

invMod :: Integral a => a -> a -> Maybe a
invMod a n =
    let (x, y) = bezout a n
        g = a * x + n * y in
    if g /= 0 && 1 `mod` g == 0
    then Just (x `mod` n)
    else Nothing

-- bezout a b = (x,y) where a*x + b*y = gcd(a, b)
bezout :: (Eq a, Integral a) => a -> a -> (a, a)
bezout _ 0 = (1, 0)
bezout a b =
    -- r = a - q * b
    -- x' * b + y' * r = gcd(a, b)
    -- x' * b + y' * (a - q * b) = gcd(a, b)
    -- y' * a + (x' - q * y') b = gcd(a, b)
    let (q, r) = a `divMod` b in
    if r == 0
    then (0, 1)
    else let (x', y') = bezout b r in (y', x' - q * y')

modForm :: Integral a => Ratio a -> a -> Maybe a
modForm (a :% b) n = fmap (\x -> (a * x) `mod` n) (invMod b n)
