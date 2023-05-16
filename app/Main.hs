module Main where

import Functions (encrypt, decrypt, rand, prime)
import System.Random

main :: IO ()
main = do
        r <- getStdGen
        let (r1, r2) = rand r
            p = 7--prime r1
            q = 11--prime r2
            n = p * q
            m = (p-1) * (q-1)
            e = prime $ (+) 1 $ max p q
        print $ (p, q, n, m, e)
