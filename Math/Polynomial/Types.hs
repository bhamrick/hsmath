module Math.Polynomial.Types where

import Math.Polynomial.Operations
import Test.QuickCheck

newtype P a = P [a]
    deriving Show

instance Arbitrary a => Arbitrary (P a) where
    arbitrary = P <$> arbitrary
    shrink (P as) = P <$> shrink as

polyVar :: Num a => P a
polyVar = P [0, 1]

degree :: P a -> Int
degree (P as) = length as

evaluate :: Num a => P a -> a -> a
evaluate (P as) x = _evalP as x

constPoly :: (Eq a, Num a) => a -> P a
constPoly c = if c == 0 then P [] else P [c]

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

instance Functor P where
    fmap f (P as) = P (fmap f as)

instance Foldable P where
    foldMap f (P as) = foldMap f as

instance Traversable P where
    traverse f (P as) = P <$> traverse f as

infixl 7 .*
(.*) :: (Eq a, Num a) => a -> P a -> P a
c .* P as = if c == 0 then P [] else P (map (*c) as)
