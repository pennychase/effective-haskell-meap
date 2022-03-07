module HCat ( runHCat ) where

import qualified System.Environment as Env

runHCat :: IO ()
runHCat = handleArgs >>= displayMessage
    where
        displayMessage parsedArgument = 
            case parsedArgument of
                Left errMsg -> putStrLn $ "Error: " <> errMsg
                Right fname -> putStrLn $ "Opening file: " <> fname

handleArgs :: IO (Either String FilePath)
handleArgs = parseArgs <$> Env.getArgs
    where
        parseArgs arguments =
            case arguments of
                []      -> Left "No filename provided"
                [fname] -> Right fname
                _       -> Left "Multiple files are not supported"