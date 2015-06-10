module Math.Modular where

modPow :: Integral a => a -> Integer -> a -> a
modPow _ 0 _ = 1
modPow a e n =
    let x = modPow a (e `div` 2) n
    in
    if even e
    then (x * x) `mod` n
    else (a * x * x) `mod` n
