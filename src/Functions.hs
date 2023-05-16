module Functions where

import System.Random
import Math.NumberTheory.Primes

encrypt :: Integer -> Integer -> Integer
encrypt plain key = plain ^ key

decrypt :: Integer -> Integer -> Integer
decrypt cipher key = cipher ^ key

rand :: StdGen -> (Integer, Integer)
rand gen = let x:y:xs = randoms gen
            in (abs x,  abs y)

prime :: Integer -> Integer
prime n = unPrime $ nextPrime n