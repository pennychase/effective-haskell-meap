module MyCat where

import System.Environment
import qualified Data.Map as Map

-- Chapter 7 Bonus Exercises

main :: IO ()
main = 
    -- myCat
    replaceAndCat

-- Exercise 1: Read file (command line arg) and print to screen
myCat :: IO ()
myCat = getArgs >>= (\args -> readFile (head args)) >>= putStrLn


-- Exercise 2: Read file and write it with some words replaced
-- Arg 1 is file of replacement words, Arg 2 is file

replaceWord :: Map.Map String String -> String -> String 
replaceWord db str =
    case Map.lookup str db of
        Nothing -> str
        Just str' -> str'

replaceAll :: Map.Map String String -> String -> String 
replaceAll db input =
    unwords $ map (replaceWord db) (words input)

mkDB :: String -> Map.Map String String
mkDB input = foldr mkEntry Map.empty (lines input)
    where 
        mkEntry :: String -> Map.Map String String -> Map.Map String String
        mkEntry line db =
            let
                [input, replace] = words line
            in Map.insert input replace db

replaceAndCat :: IO ()
replaceAndCat = 
    files >>= print . replaceAll'
    where
        files = getArgs >>= mapM readFile
        replaceAll' files = replaceAll (mkDB (head files)) (files !! 1)

