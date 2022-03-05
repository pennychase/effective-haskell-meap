module LazyIODemo where

import Text.Printf

-- CHapter 7 Examples

-- Since screamIntotheVoid is never looked at, the IO action is never evaluated
ignoreUnevaluatedIO :: IO ()
ignoreUnevaluatedIO =
    let screamIntotheVoid = putStrLn "quack"
    in return ()

-- Exception never raised although IO actions are evaluated
lazyIODemo :: IO ()
lazyIODemo =
    let
        sayHello :: IO ()
        sayHello = putStrLn "Hello"

        raiseAMathError :: IO Int
        raiseAMathError = putStrLn "I'm part of raiseAMathError" >> return (1 `div` 0)
    in sayHello
    >> raiseAMathError 
    >> sayHello

-- Too many open file handles because we try to open all the files before printing anything out
makeAndReadFile :: Int -> IO String
makeAndReadFile fnumber =
    let fname = printf "/tmp/test/%d" fnumber
    in writeFile fname fname >> readFile fname

unsafe :: IO ()
unsafe = 
    let files = mapM makeAndReadFile [1..3000] :: IO [String]
    in files >>= (putStrLn . show)

-- Solution: Have each IO action gets the files and prints it, so we can close 
makeAndShow :: Int -> IO ()
makeAndShow n =
    makeAndReadFile n >>= putStrLn

safe :: IO ()
safe = mapM_ makeAndShow [1 .. 3000]
