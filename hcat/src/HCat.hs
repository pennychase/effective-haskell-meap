{-# LANGUAGE LambdaCase #-}

module HCat ( runHCat ) where

import qualified System.Environment as Env
import qualified Control.Exception as Exception
import qualified System.IO.Error as IOError

-- Error Handling version 1
-- Use withErrorHandling to provide a context, while focusing on handleArgs (the real work of runHCat)
runHCat :: IO ()
runHCat =
    withErrorHandling $
        handleArgs
        >>= \case
            Left err -> putStrLn $ "Error: " <> err
            Right fname -> readFile fname >>= putStrLn
    where
        withErrorHandling :: IO () -> IO ()
        withErrorHandling ioAction = Exception.catch ioAction handleErr

        handleErr :: IOError -> IO ()
        handleErr e = putStrLn "I ran into an error!" >> print e


handleArgs :: IO (Either String FilePath)
handleArgs = parseArgs <$> Env.getArgs
    where
        parseArgs arguments =
            case arguments of
                []      -> Left "No filename provided"
                [fname] -> Right fname
                _       -> Left "Multiple files are not supported"