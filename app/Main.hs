module Main where

import Control.Parallel.Strategies

maxDiv :: Int -> Int
maxDiv = ceiling . sqrt . fromIntegral

isPrime' n = n== 2 || not (loop 2 (maxDiv n))
 where loop i j = n `mod` i == 0 || (i<j && (loop (i+1) j))


findPrimes:: Int->Int->[Int]
findPrimes start stop = loop start stop []
 where loop i j acc = if i>j then
                          acc
                      else
                          loop (i+1) j (if isPrime' i then i:acc else acc)

loop2:: Int->Int->Int->[(Int,Int)]->[(Int,Int)]
loop2 start stop step acc = if (start+step) >= stop then
                                (start,stop):acc
                            else
                                loop2 (start+step+1) stop step ((start,start+step):acc)
                                
parMap2 strat f lst = map f lst `using` parBuffer 8190 strat
splitter start stop = loop2 start stop ((stop-start) `div` 8190) []
findPrimesUnCurried = uncurry findPrimes
findPrimesPar start stop = parMap2 rdeepseq findPrimesUnCurried $ splitter start stop
main=writeFile "primes.txt" (show $ findPrimesPar 2 32_000_000)
