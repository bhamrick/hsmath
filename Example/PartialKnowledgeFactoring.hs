module Main where

import Math.Primes
import Math.Polynomial
import Math.Polynomial.Coppersmith

main :: IO ()
main = do
    let numBits = 512
        numUnknownBits = 168
    putStrLn $ "Generating two random " ++ show numBits ++ " bit primes."
    p <- randomPrime numBits
    q <- randomPrime numBits
    let n = p*q
    putStrLn $ "Their product is " ++ show n
    let partialP = (\x -> x - x `mod` 2^numUnknownBits) (max p q)
    putStrLn $ "The high " ++ show (numBits - numUnknownBits) ++ " bits of the larger factor are " ++ show partialP
    putStrLn $ "Attempting to find the remaining " ++ show numUnknownBits ++ " bits..."
    case partialFactor n partialP (2^numUnknownBits) of
        Nothing -> putStrLn "Did not successfully find the low bits."
        Just foundFactor -> putStrLn $ "Success! The factor is " ++ show foundFactor
