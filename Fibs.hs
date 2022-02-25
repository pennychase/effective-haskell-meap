module Fibs where

-- Chapter 2: Fibonacci Series

fib n 
    | n == 0 = 0
    | n == 1 = 1
    | otherwise = (fib $ n - 1) + (fib $ n - 2)

fibs = map fib [0 ..]

-- Lazy generator for fibonaci

fibs' =
    0 : 1 : nextFibs fibs (tail fibs)
    where
        nextFibs (a:as) (b:bs) = (a+b) : nextFibs as bs