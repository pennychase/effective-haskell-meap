module Chapter3 where

import Data.List

incrementAndPrint :: Int -> (Int -> String) -> String
incrementAndPrint num printer = printer (num + 1)

incrementAndPrint' :: Int -> (Int -> (Int -> String) -> String) -> String
incrementAndPrint' num f = f (num + 1) show


sumBiggest :: [[Int]] -> String
sumBiggest allNums = 
    let
        getBiggests :: [Int] -> [Int]
        getBiggests = last . group . sort

        getSmallests :: [Int] -> [Int]
        getSmallests = head . group . sort

        allBiggests :: [[Int]]
        allBiggests = map getBiggests allNums

        allSmallests :: [[Int]]
        allSmallests = map getSmallests allNums

        sizePairs :: [([Int], [Int])]
        sizePairs = zip allBiggests allSmallests

        differences :: ([Int], [Int]) -> Int
        differences p = sum (fst p) - sum (snd p)

        differences' :: [String]
        differences' = map (show . differences) sizePairs
    in intercalate "," differences'

showBiggest =
    let
        biggestInfo = sumBiggest [[1,1,2,3,4,4],[1,2,5,5],[-1,-2,5,-10,5]]
    in print $ "sumBiggest says: " <> biggestInfo

