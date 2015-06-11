module Math.Polynomial.Hensel where

import Control.Monad
import Math.Polynomial.Types
import Math.Modular

-- |Implementation of Hensel's lemma / p-adic Newton's method
-- Given a polynomial f and an integer x such that f(x) = 0 mod p^r
-- and f'(x) /= 0 mod p, computes a new integer x' such that
-- f(x') = 0 mod p^(r+1) and x = x' mod p^r
--
-- f(x + a*p^r) = 0 mod p^(2r)
-- f(x) + a*p^r*f'(x) = 0 mod p^(2r)
-- (f(x) / p^r) + a*f'(x) = 0 mod p^r
-- a = -(f(x) / p^r) * (f'(x)^(-1)) mod p^r
-- x' = x + a*p^r
henselLift :: P Integer -> Integer -> Integer -> Int -> Maybe Integer
henselLift f x p r = do
    let m = p^r
        df = derivative f
        (s, t) = (evaluate f x) `divMod` m
    when (t /= 0) Nothing
    dfInv <- invMod (evaluate df x) m
    return $ (x - (s * dfInv) * m) `mod` (m^2)
