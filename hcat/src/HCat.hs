{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module HCat ( runHCat ) where

import qualified System.Environment as Env
import qualified Control.Exception as Exception
import qualified System.IO.Error as IOError

-- Error Handling version 2
-- Unify error handling approach by throwing an exception after handle args (turning the Either result into an IO Error)

runHCat :: IO ()
runHCat =
    withErrorHandling $
        handleArgs
            >>= eitherToErr
            >>= readFile
            >>= putStrLn
    where
        withErrorHandling :: IO () -> IO ()
        withErrorHandling ioAction = Exception.catch ioAction $
            \e -> print "I ran into an error!" >> print @IOError e


handleArgs :: IO (Either String FilePath)
handleArgs = parseArgs <$> Env.getArgs
    where
        parseArgs arguments =
            case arguments of
                []      -> Left "No filename provided"
                [fname] -> Right fname
                _       -> Left "Multiple files are not supported"

eitherToErr :: Show a => Either a b -> IO b
eitherToErr (Right a) = return a
eitherToErr (Left e) = Exception.throwIO . IOError.userError $ show e