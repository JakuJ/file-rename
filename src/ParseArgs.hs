module ParseArgs (
    exit, die, buildTransformer
) where

import System.IO
import System.Console.GetOpt
import System.Exit (exitWith, ExitCode(..))
import Data.List (nub)
import Control.Monad (when)

import Parser (parseFormat)
import Format (FileInfo (..), getTransformer)

exit, die :: IO a
exit = exitWith ExitSuccess
die = exitWith (ExitFailure 1)

data Flag
    = Extension   -- -e
    | Version     -- -v
    | Help        -- --help
    deriving (Eq, Enum, Show)

flags :: [OptDescr Flag]
flags =
    [Option ['e'] [] (NoArg Extension)
            "Don't preserve extensions"
    ,Option ['v'] ["version"] (NoArg Version)
            "Print version number"
    ,Option ['h'] ["help"] (NoArg Help)
            "Print this help message"
    ]

parseArgs :: [String] -> IO ([Flag], String)
parseArgs argv = case getOpt Permute flags argv of
    (args, fs, []) -> do
        let format = if null fs then "" else concat fs
        when (Help `elem` args) $ hPutStrLn stderr usage >> exit
        when (Version `elem` args) $ hPutStrLn stderr version >> exit
        return (nub args, format)
    (_, _, errs) -> do
        hPutStrLn stderr (concat errs ++ usage)
        die   
    where 
        header = "Usage: rename [-evh] [format ...]"
        version = "rename version 1.1"
        usage = usageInfo header flags

buildTransformer :: [String] -> IO (FileInfo -> FilePath)
buildTransformer argv = do
    (flags, format) <- parseArgs argv
    fun <- case parseFormat format of
        Left err -> print err >> die
        Right fmts -> getTransformer fmts
    
    return $ if not (Extension `elem` flags)
        then \fi -> fun fi ++ extension fi
        else fun