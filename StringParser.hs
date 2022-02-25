module StringParser where

import Data.List

-- StringParser data type
newtype StringParser = 
    StringParser { runStringParser :: String -> (String, String) }

-- Parsers
takeChars :: Int -> StringParser
takeChars n = StringParser $ \someString ->
    splitAt n someString

getNextWord :: StringParser
getNextWord = StringParser $ \someString ->
    case break (== ' ') someString of
        (nextWord, "") -> (nextWord, "")
        (nextWord, rest) -> (nextWord, tail rest)  -- the space is part of rest, so need to strip it off

-- Combine parsers
combineParsers :: StringParser -> StringParser -> StringParser
combineParsers firstParser secondParser = StringParser $ \someString ->
    let (_firstPart, firstResult) = runStringParser firstParser someString
    in runStringParser secondParser firstResult

-- Run a parser
parseString :: StringParser -> String -> String
parseString parser inputString = fst $ runStringParser parser inputString

