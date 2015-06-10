module Math.Lattice.LLL where

import Data.Ratio

infixl 6 .+
(.+) :: Num a => [a] -> [a] -> [a]
[] .+ bs = bs
as .+ [] = as
(a:as) .+ (b:bs) = (a + b) : (as .+ bs)

infixl 6 .-
(.-) :: Num a => [a] -> [a] -> [a]
[] .- bs = map negate bs
as .- [] = as
(a:as) .- (b:bs) = (a - b) : (as .- bs)

infixl 7 .*
(.*) :: Num a => a -> [a] -> [a]
c .* as = map (* c) as

dot :: Num a => [a] -> [a] -> a
dot as bs = sum (zipWith (*) as bs)

gramSchmidt :: Fractional a => [[a]] -> [[a]]
gramSchmidt = reverse . go []
    where
    go bs [] = bs
    go bs (v:vs) = go (gramSchmidtStep bs v : bs) vs

gramSchmidtStep :: Fractional a => [[a]] -> [a] -> [a]
gramSchmidtStep [] v = v
gramSchmidtStep (b:bs) v =
    let mu = dot b v / dot b b
        v' = v .- mu .* b
    in gramSchmidtStep bs v'

lll :: RealFrac a => [[a]] -> [[a]]
lll = reverse . go []
    where
    delta :: Fractional a => a
    delta = 3/4
    go :: RealFrac a => [([a], [a])] -> [[a]] -> [[a]]
    go basis [] = map fst basis
    go [] (v:vs) = go [(v, v)] vs
    go basis@((b,b'):bs) (v:vs) =
        let v' = lllReduceStep basis v
            v'' = gramSchmidtStep (map snd basis) v'
            mu = dot b' v' / dot b' b'
        in
        if dot v'' v'' >= (delta - mu^2) * dot b' b'
        then go ((v',v''):basis) vs
        else go bs (v':b:vs)

lllReduceStep :: RealFrac a => [([a], [a])] -> [a] -> [a]
lllReduceStep [] v = v
lllReduceStep ((b,b'):bs) v =
    let mu = dot b' v / dot b' b'
    in lllReduceStep bs (v .- fromInteger (round mu) .* b)
