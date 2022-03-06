module Chapter7 where

import Text.Read
import System.Environment

-- Exercises 1 and 2
main :: IO ()
main = 
    -- sumArgs <$> getArgs >>= print
    opArgs <$> getArgs >>= print

sumArgs :: [String] -> Maybe Int 
sumArgs strArgs =
    let
        intArgs = mapM readMaybe strArgs
    in sum <$> intArgs

opArgs :: [String] -> Maybe Int 
opArgs strArgs = 
    case strArgs of
        [] -> Nothing
        op:rest ->
            let intArgs = mapM readMaybe rest
            in (eval op) <$> intArgs

eval :: String -> ([Int] -> Int)
eval op =
    case op of
        "+" -> sum
        "-" -> foldl1 (-)
        "*" -> product

-- Exercise 3
foo :: IO (IO String)
foo = return foo'
    where
        foo' :: IO String
        foo' = return "foo"

-- To print foo: 
-- foo >>= (\x -> x) >>= (\y -> print y)
-- OR foo >>= id >>= print

-- Exercise 4
unwrapIO :: IO (IO a) -> IO a
unwrapIO action = action >>= id

-- Exercise 5

ioAction :: (a -> IO a) -> [a] -> [IO a]
ioAction f xs = map f xs

ioSequence:: [IO a] -> IO [a]
ioSequence actions = sequence actions