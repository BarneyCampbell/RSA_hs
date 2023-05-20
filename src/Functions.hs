module Functions where

import Math.NumberTheory.Primes ( nextPrime, Prime(unPrime) )

encrypt :: Integer -> Integer -> Integer -> Integer
encrypt plain key base = (plain ^ key) `mod` base

decrypt :: Integer -> Integer -> Integer -> Integer
decrypt cipher key base = (cipher ^ key) `mod` base

prime :: Integer -> Integer
prime n = unPrime $ nextPrime n

-- Calculate the multiplicative inverse, d
-- Needs a proper formula but for small numbers can check until reached.
multInv :: Integer -> Integer -> Integer -> Integer
multInv e m d
        | mod (e * d) m == 1 = d
        | otherwise = multInv e m (d+1)