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

instance Num a => Num (P a) where
    P as + P bs = P (_addP as bs)
    P as - P bs = P (_subP as bs)
    P as * P bs = P (_hybridMultP as bs)
    negate (P as) = P (_negateP as)
    abs p = p
    signum _ = 1
    fromInteger c = P [fromInteger c]
