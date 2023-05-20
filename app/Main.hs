module Main where

import Functions (encrypt, decrypt, prime, multInv)
import System.Random ( getStdGen, Random(randoms), StdGen )
import System.Environment (getArgs)

main :: IO ()
main = do
        args <- getArgs
        r <- getStdGen
        let (r1, r2) = rand r
            (secret, direction) = getEncDec args
        if length args == 0 then
            print "Not enough arguments!"
        else
            output direction (r1, r1) args
                
output :: Integer -> (Integer, Integer) -> [String] -> IO ()
output 4 _ _        = print "Error"
output 3 (x,y) _    = print $ keygen x y
output 0 _ args     = print $ encrypt $ getKeys args
output 1 _ args     = print $ decrypt $ getKeys args


rand :: StdGen -> (Integer, Integer)
rand gen = let x:y:z:xs = randoms gen
            in if x == y
                then ((abs x) `mod` 1000, (abs z) `mod` 1000)
                else ((abs x) `mod` 1000, (abs y) `mod` 1000)

keygen :: Integer -> Integer -> (Integer, Integer, Integer)
keygen r1 r2 = let
                    p = prime r1
                    q = prime r2
                    n = p * q
                    m = (p-1) * (q-1)
                    e = prime $ (+) 1 $ max p q
                    d = multInv e m $ max p q -- e * d mod m = 1
                in (n, e, d)

readBool :: String -> Integer
readBool direction
    | direction == "encrypt" = 0
    | direction == "decrypt" = 1

getEncDec :: [String] -> (Integer, Integer)
getEncDec ("keygen":[]) = (0, 3)
getEncDec (x:[]) = (0, 4)
getEncDec (direction:value:xs) = (read value :: Integer, readBool direction)

getKeys :: [String] -> (Integer, Integer, Integer)
getKeys (_:_:value:k:base:xs) = (read value :: Integer, read k :: Integer, read base :: Integer)