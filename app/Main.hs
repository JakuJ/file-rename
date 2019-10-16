module Main where

import System.Exit (exitWith, ExitCode(..))
import System.Environment (getArgs)
import System.Directory (listDirectory)
import Data.List (sort)

import Format (getTransform, FileInfo)
import Parser (parseFormat)
import Rename (newFilepaths, renameFiles)

main :: IO ()
main = do
    files <- sort <$> listDirectory "."
    transformer <- getArgs >>= parseArgs
    renameFiles $ zip files $ newFilepaths files transformer

parseArgs :: [String] -> IO (FileInfo -> FilePath)
parseArgs ["-h"]   = usage >> exit
parseArgs ["-v"]   = version >> exit
parseArgs [format] = case parseFormat format of
                        Left err -> print err >> die
                        Right formats -> getTransform formats
parseArgs _        = usage >> die

usage, version :: IO ()
usage   = putStrLn "Usage: rename [-vh] [format ..]"
version = putStrLn "file-rename 1.0"

exit, die :: IO a
exit    = exitWith ExitSuccess
die     = exitWith (ExitFailure 1)