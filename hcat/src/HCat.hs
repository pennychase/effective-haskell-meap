{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}

module HCat ( runHCat ) where

import qualified Control.Exception as Exception
import qualified Data.ByteString as BS
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified Data.Time.Clock as Clock
import qualified Data.Time.Format as TimeFormat
import qualified Data.Time.Clock.POSIX as PosixClock
import qualified System.Directory as Directory
import qualified System.Environment as Env
import System.Info
import System.IO
import qualified System.IO.Error as IOError
import qualified System.Process as Process
import qualified Text.Printf as Printf


-- Top level

runHCat :: IO ()
runHCat = do
    files <- eitherToErr =<< handleArgs
    mapM_ runHCat' files
    where
        runHCat' targetFilePath = do 
            contents <- TextIO.hGetContents =<< openFile targetFilePath ReadMode 
            termSize <- getTerminalSize
            hSetBuffering stdout NoBuffering
            finfo <- fileInfo targetFilePath
            let pages = paginate termSize finfo contents
            showPages pages []

-- Unify error handling approach by throwing an exception after handle args (turning the Either result into an IO Error)
handleArgs :: IO (Either String [FilePath])
handleArgs = parseArgs <$> Env.getArgs
    where
        parseArgs arguments =
            case arguments of
                []      -> Left "No filename provided"
                files -> Right files

eitherToErr :: Show a => Either a b -> IO b
eitherToErr (Right a) = return a
eitherToErr (Left e) = Exception.throwIO . IOError.userError $ show e

-- Get user input to page or quit
data Command = Continue | Backwards | Cancel deriving (Eq, Show)

getContinue :: IO Command
getContinue =
    hSetBuffering stdin NoBuffering 
    >> hSetEcho stdin False
    >> hGetChar stdin
    >>= \case
        ' ' -> return Continue
        'b' -> return Backwards
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

showPages :: [Text.Text] -> [Text.Text] -> IO ()
showPages [] _ = return ()
showPages (page:pages) stack =
    clearScreen
    >> TextIO.putStrLn page
    >> getContinue
    >>= \case
            Continue -> showPages pages (page:stack)
            Backwards -> if null stack 
                            then showPages (page : pages) (tail stack)
                            else showPages ((head stack) : page : pages) (tail stack)
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
        tputScreenDimensions = do
            lines <- getDim "lines" 25
            cols <- getDim "cols" 80
            return $ ScreenDimensions lines cols

        getDim :: String -> Int -> IO Int
        getDim dim def = do
            Exception.catch (let str = Process.readProcess "tput" [dim] "" in seq str $ readDim str) handleErr
            where
                handleErr :: Exception.SomeException -> IO Int
                handleErr _ = do
                    return def

                readDim :: IO String -> IO Int
                readDim str = do
                    str' <- str
                    let str'' = Text.unpack . Text.strip. Text.pack $ str'
                    Exception.catch (let i = read str'' in seq i $ return i) handleErr

-- Paginate according to screen size
-- Need to leave room for status bar (so each page is of length rows - 1)
-- and pad with empty lines to fill the vertical space
paginate :: ScreenDimensions -> FileInfo -> Text.Text -> [Text.Text]
paginate (ScreenDimensions rows cols) finfo text =
    let 
        rows' = rows - 1
        wrappedLines = concatMap (wordWrap cols) (Text.lines text)
        pages = map (Text.unlines . padTo rows') $ groupsOf rows' wrappedLines
        pageCount = length pages
        statusLines = map (formatFileInfo finfo cols pageCount) [1 .. pageCount]
    in zipWith (<>) pages statusLines
    where
        padTo :: Int -> [Text.Text] -> [Text.Text]
        padTo lineCount rowsToPad =
            take lineCount $ rowsToPad <> repeat ""


-- Status Line

data FileInfo = FileInfo 
    { filePath :: FilePath 
    , fileSize :: Int 
    , fileMTime :: Clock.UTCTime 
    , fileReadable :: Bool 
    , fileWriteable :: Bool 
    , fileExecutable :: Bool 
    } deriving Show

fileInfo :: FilePath -> IO FileInfo
fileInfo filePath = do
    perms <- Directory.getPermissions filePath
    mtime <- Directory.getModificationTime filePath
    size <- Text.length <$> TextIO.readFile filePath
    return FileInfo
        { filePath = filePath
        , fileSize = size
        , fileMTime = mtime
        , fileReadable = Directory.readable perms
        , fileWriteable = Directory.writable perms
        , fileExecutable = Directory.executable perms 
        }

formatFileInfo :: FileInfo -> Int -> Int -> Int -> Text.Text
formatFileInfo FileInfo {..} maxWidth totalPages currentPage =
    let
        timestamp = TimeFormat.formatTime TimeFormat.defaultTimeLocale "%F %T" fileMTime
        permissionString =
            [ if fileReadable then 'r' else '-'
            , if fileWriteable then 'w' else '-'
            , if fileExecutable then 'x' else '-'
            ]
        statusLine = Text.pack $
            Printf.printf
                "%s / permissions: %s / %d bytes / modified: %s / page: %d of %d"
                filePath
                permissionString
                fileSize
                timestamp
                currentPage
                totalPages
    in invertText (truncateStatus statusLine)
    where
        invertText inputStr =
            let
                reverseVideo = "\^[[7m"
                resetVideo = "\^[[0m"
            in reverseVideo <> inputStr <> resetVideo
        truncateStatus statusLine
            | maxWidth <= 3 = ""
            | Text.length statusLine > maxWidth = Text.take (maxWidth - 3) statusLine <> "..."
            | otherwise = statusLine


