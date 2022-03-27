{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module HCat ( runHCat ) where

import qualified Control.Exception as Exception
import qualified Data.ByteString as BS
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified System.Environment as Env
import System.IO
import qualified System.IO.Error as IOError
import System.Info
import qualified System.Process as Process

-- Top level

runHCat :: IO ()
runHCat = 
    handleArgs
    >>= eitherToErr
    >>= flip openFile ReadMode 
    >>= TextIO.hGetContents
    >>= \contents ->
        getTerminalSize >>= \termSize ->
            let pages = paginate termSize contents
            in showPages pages

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

-- Get user input to page or quit
data ContinueCancel = Continue | Cancel deriving (Eq, Show)

getContinue :: IO ContinueCancel
getContinue =
    hSetBuffering stdin NoBuffering 
    >> hSetEcho stdin False
    >> hGetChar stdin
    >>= \case
        ' ' -> return Continue
        'q' -> return Cancel
        _  -> getContinue

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

-- Show pages

showPages :: [Text.Text] -> IO ()
showPages [] = return ()
showPages (page:pages) =
    clearScreen
    >> TextIO.putStrLn page
    >> getContinue
    >>= \case
            Continue -> showPages pages
            Cancel -> return ()

clearScreen :: IO ()
clearScreen = BS.putStr "\^[[1J\^[[1;1H"

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

-- Paginate according to screen size
-- Can test with: 46(Screen Dimensions 5 10) "Some ling text ..."
paginate :: ScreenDimensions -> Text.Text -> [Text.Text]
paginate (ScreenDimensions rows cols) text =
    let 
        unwrappedLines = Text.lines text
        wrappedLines = concatMap (wordWrap cols) unwrappedLines
        pageLines = groupsOf rows wrappedLines
    in map Text.unlines pageLines
