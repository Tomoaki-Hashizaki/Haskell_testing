{-
    For any natural numbers a and b, if a and b are coprime, then a + b and ab are also coprime.
    This is a theorem; therefore, this program never stops. 
-}

import Debug.Trace

pairs :: [(Int, Int)]
pairs =  [traceShow (x + y, x, y) (x, y) | u <- [4..], x <- [2..u], y <-[2..u] , u == x + y && x < y]

coprimes :: (Int, Int) -> Bool
coprimes (a, b) = gcd a b == 1

main :: IO ()
main = putStrLn $ show $ head (filter (\(x, y) -> not (coprimes (x + y, x * y))) (filter coprimes pairs))
