module Math.Primes where

import Control.Monad
import Control.Monad.Random
import Math.Modular

millerRabinStep :: Integer -> Integer -> Bool
millerRabinStep a n =
    if gcd a n /= 1
    then a `mod` n == 0
    else
        let (r, d) = removeTwos (n-1)
            base = modPow a d n
            powers = take (fromInteger r) $ iterate (\x -> (x * x) `mod` n) base
        in
        base == 1 || any (== n - 1) powers

removeTwos :: Integer -> (Integer, Integer)
removeTwos 0 = (0, 0)
removeTwos n = if even n
    then let (r, d) = removeTwos (n `div` 2) in (r+1, d)
    else (0, n)

millerRabin :: MonadRandom m => Int -> Integer -> m Bool
millerRabin 0 _ = return True
millerRabin k n = do
    witness <- getRandomR (1, n-1)
    if millerRabinStep witness n
        then millerRabin (k-1) n
        else return False

-- randomPrime n returns a random prime with n bits
randomPrime :: MonadRandom m => Int -> m Integer
randomPrime n = do
    candidate <- fmap (\x -> 2*x + 1) (getRandomR (2^(n-2), 2^(n-1) - 1))
    isPrime <- millerRabin k candidate
    if isPrime
        then return candidate
        else randomPrime n
    where
    k = 64
