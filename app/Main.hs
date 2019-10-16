module Main where

import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(..))

import Rename (renameFiles)
import Parser (parseFormat)

main :: IO ()
main = getArgs >>= parseArgs >>= renameFiles

parseArgs :: [String] -> IO (FilePath -> FilePath)
parseArgs []       = usage >> exit
parseArgs ["-h"]   = usage >> exit
parseArgs ["-v"]   = version >> exit
parseArgs [format] = case parseFormat format of
                        Left err -> print err >> die
                        Right fun -> fun

parseArgs _        = usage >> exit

usage   = putStrLn "Usage: rename [-vh] [format ..]"
version = putStrLn "Haskell file-rename 0.1"
exit    = exitWith ExitSuccess
die     = exitWith (ExitFailure 1)