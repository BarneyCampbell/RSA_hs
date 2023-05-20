module Main where

import Functions (encrypt, decrypt, prime, multInv)
import System.Random ( getStdGen, Random(randoms), StdGen )
import System.Environment (getArgs)

main :: IO ()
main = do
        args <- getArgs
        r <- getStdGen
        let (r1, r2) = rand r
            p = prime r1
            q = prime r2
            n = p * q
            m = (p-1) * (q-1)
            e = prime $ (+) 1 $ max p q
            d = multInv e m $ max p q -- e * d mod m = 1

            secret = 15

            enc = encrypt secret e n
            dec = decrypt enc d n

        print (p, q, n, m, e, d, secret, enc, dec)

rand :: StdGen -> (Integer, Integer)
rand gen = let x:y:z:xs = randoms gen
            in if x == y
                then ((abs x) `mod` 1000, (abs z) `mod` 1000)
                else ((abs x) `mod` 1000, (abs y) `mod` 1000)
