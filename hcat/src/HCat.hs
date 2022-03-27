{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module HCat ( runHCat ) where

import qualified Control.Exception as Exception
import qualified Data.ByteString as BS
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified System.Environment as Env
import qualified System.IO.Error as IOError
import System.Info
import qualified System.Process as Process

-- Top level

runHCat :: IO ()
runHCat =
    withErrorHandling $
        handleArgs
            >>= eitherToErr
            >>= TextIO.readFile
            >>= TextIO.putStrLn
    where
        withErrorHandling :: IO () -> IO ()
        withErrorHandling ioAction = Exception.catch ioAction $
            \e -> print "I ran into an error!" >> print @IOError e

-- Error Handling version 2
-- Unify error handling approach by throwing an exception after handle args (turning the Either result into an IO Error)
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


-- Pager

-- Group lines of text into pages
groupsOf :: Int -> [a] -> [[a]]
groupsOf n [] = []
groupsOf n elems =
    let
        (hd, tl) = splitAt n elems
    in hd : groupsOf n tl

-- Word wrapping
wordWrap :: Int -> Text.Text -> [Text.Text]
wordWrap lineLength lineText
    | Text.length lineText <= lineLength = [lineText]
    | otherwise =
        let
            (candidate, nextLines) = Text.splitAt lineLength lineText
            (firstLine, overflow) = softWrap candidate (Text.length candidate - 1)
        in firstLine : wordWrap lineLength (overflow <> nextLines)
    where
        softWrap hardwrappedText textIndex
            | textIndex <= 0 = (hardwrappedText, Text.empty)
            | Text.index hardwrappedText textIndex == ' ' = 
                let
                    (wrappedLine, rest) = Text.splitAt textIndex hardwrappedText
                in (wrappedLine, Text.tail rest)
            | otherwise = softWrap hardwrappedText (textIndex - 1) 

-- Terminal dimensions

data ScreenDimensions = ScreenDimensions
    { screenRows :: Int 
    , screenColumns :: Int 
    } deriving Show

getTerminalSize :: IO ScreenDimensions
getTerminalSize =
    case System.Info.os of
        "darwin" -> tputScreenDimensions
        "linus" -> tputScreenDimensions
        _other -> pure $ ScreenDimensions 25 80
    where
        tputScreenDimensions :: IO ScreenDimensions
        tputScreenDimensions =
            Process.readProcess "tput" ["lines"] ""
            >>= \lines ->
                Process.readProcess "tput" ["cols"] ""
                >>= \cols ->
                    let lines' = read $ init lines
                        cols' = read $ init cols
                    in pure $ ScreenDimensions lines' cols'

-- Paginate accroding to screen size
-- Can test with: mapM_ TextIO.putStrLn $ paginate (Screen Dimensions 5 10) "Some ling text ..."
paginate :: ScreenDimensions -> Text.Text -> [Text.Text]
paginate (ScreenDimensions rows cols) text =
    let 
        unwrappedLines = Text.lines text
        wrappedLines = concatMap (wordWrap cols) unwrappedLines
        pageLines = groupsOf rows wrappedLines
    in map Text.unlines pageLines
