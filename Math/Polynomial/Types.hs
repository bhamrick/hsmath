module Math.Polynomial.Types where

import Math.Polynomial.Operations

newtype P a = P [a]
    deriving Show

polyVar :: Num a => P a
polyVar = P [0, 1]

degree :: P a -> Int
degree (P as) = length as

evaluate :: Num a => P a -> a -> a
evaluate (P as) x = _evalP as x

coefficients :: P a -> [a]
coefficients (P as) = as

instance (Eq a, Num a) => Num (P a) where
    P as + P bs = P (_simplify $ _addP as bs)
    P as - P bs = P (_simplify $ _subP as bs)
    P as * P bs = P (_simplify $ _hybridMultP as bs)
    negate (P as) = P (_negateP as)
    abs p = p
    signum _ = 1
    fromInteger c = P $ _simplify [fromInteger c]

instance (Eq a, Num a) => Eq (P a) where
    p == q = coefficients (p - q) == []
