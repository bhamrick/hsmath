module Math.Polynomial.ModularRoots where

import Control.Monad.Random
import Data.List
import Data.Maybe
import Math.Modular
import Math.Polynomial.Modular
import Math.Polynomial.Operations
import Math.Polynomial.Types
import System.Random

rootsModP :: P Integer -> Integer -> [Integer]
rootsModP f p = case traverse (flip modForm p) . monicMultiple . fmap fromInteger $ f of
    Nothing -> []
    Just (P []) -> [0..p-1]
    Just f'@(P (a:_)) ->
        let g = modularModPow polyVar p f p - polyVar
            f'' = modularPolyGcd f' g p
        in sort $ evalRand (_smallDegreeRootsModP f'' p) (mkStdGen (fromInteger a))

_smallDegreeRootsModP :: MonadRandom m => P Integer -> Integer -> m [Integer]
_smallDegreeRootsModP (P []) p = return [0..p-1]
_smallDegreeRootsModP (P [c]) p = return []
_smallDegreeRootsModP (P [b, a]) p = return $ case invMod a p of
    Nothing -> []
    Just a' -> [((-b) * a') `mod` p]
_smallDegreeRootsModP f p = do
    d <- getRandomR (0, p-1)
    let g = modularModPow (polyVar - fromInteger d) ((p - 1) `div` 2) f p - 1
        f' = modularPolyGcd f g p
        f'' = fst (modularDivModPoly f f' p)
    if degree f' > 1 && degree f' < degree f
        then do r1 <- _smallDegreeRootsModP f' p
                r2 <- _smallDegreeRootsModP f'' p
                return $ r1 ++ r2
        else _smallDegreeRootsModP f p
