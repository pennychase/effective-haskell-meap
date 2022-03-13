module FizzBuzz where

fizzbuzz :: Int -> String 
fizzbuzz n = concatMap fizzbuzzOne[1 .. n]
    where
        fizzbuzzOne n
            | n `mod` 15 == 0   = "fizzbuzz"
            | n `mod` 3 == 0    = "fizz"
            | n `mod` 5 == 0    = "buzz"
            | otherwise         = show n