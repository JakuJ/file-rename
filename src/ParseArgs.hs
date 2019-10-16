module ParseArgs (
    parseArgs, 
    buildTransformer, buildRenamer
) where

import System.IO (hPutStrLn, stderr)
import System.Console.GetOpt (OptDescr (..), ArgDescr (..), ArgOrder (..), getOpt, usageInfo)
import System.Exit (exitWith, ExitCode (..))
import System.Directory (renameFile)
import Control.Monad (when, guard)
import System.Posix.Files (fileExist)

import Parser (parseFormat)
import Format (FileInfo (..), getTransformer)

exit, die :: IO a
exit = exitWith ExitSuccess
die = exitWith (ExitFailure 1)

data Flag = Extension | Dry | Version | Help
    deriving (Eq, Enum, Show)

flags :: [OptDescr Flag]
flags =
    [Option ['e'] [] (NoArg Extension)
            "Don't preserve extensions"
    ,Option ['d'] ["dry-run"] (NoArg Dry)
            "Don't rename files, just print new names"
    ,Option ['v'] ["version"] (NoArg Version)
            "Print version number"
    ,Option ['h'] ["help"] (NoArg Help)
            "Print this help message"
    ]

parseArgs :: [String] -> IO ([Flag], String)
parseArgs argv = case getOpt Permute flags argv of
    (args, fs, []) -> do
        when (Help `elem` args) $ exitWith usage
        when (Version `elem` args) $ exitWith version
        when (null fs) $ failWith usage
        return (args, concat fs)
    (_, _, errs) -> failWith $ concat errs ++ usage
    where 
        exitWith msg = hPutStrLn stderr msg >> exit
        failWith msg = hPutStrLn stderr msg >> die
        header = "Usage: rename [-evh] [format ...]"
        version = "rename version 1.2"
        usage = usageInfo header flags

buildTransformer :: [Flag] -> String -> IO (FileInfo -> FilePath)
buildTransformer flags format = do
    fun <- case parseFormat format of
        Left err -> print err >> die
        Right fmts -> getTransformer fmts
    
    return $ if not (Extension `elem` flags)
        then \fi -> fun fi ++ extension fi
        else fun

buildRenamer :: [Flag] -> (FilePath -> FilePath -> IO ())
buildRenamer flags = 
    if (Dry `elem` flags)
        then log
        else \old new -> do
            guard =<< not <$> fileExist new
            log old new
            renameFile old new
    where
        log old new = putStrLn $ old ++ " -> " ++ new
