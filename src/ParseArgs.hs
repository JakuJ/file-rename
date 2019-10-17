module ParseArgs (
    parseArgs,
    listFilenames,
    buildTransformer,
    buildRenamer
) where

import System.IO (hPutStrLn, stderr)
import System.Console.GetOpt (OptDescr (..), ArgDescr (..), ArgOrder (..), getOpt, usageInfo)
import System.Exit (exitWith, ExitCode (..))
import System.Directory (renameFile, listDirectory, doesPathExist, doesFileExist)
import System.FilePath.Posix ((</>), dropExtension)
import Control.Monad (when, guard, liftM, filterM)
import Data.List (sort, nub)

import Parser (parseFormat)
import Format (FileInfo (..), getTransformer)

exit, die :: IO a
exit = exitWith ExitSuccess
die = exitWith (ExitFailure 1)

data Flag 
    = Directory {directory :: String} 
    | Extension | Dry | Version | Help
    deriving (Eq, Show)

flags :: [OptDescr Flag]
flags =
    [Option ['d'] ["dir"] (ReqArg Directory "DIR")
            "Directory to rename files in - defaults to the current one"
    ,Option ['e'] [] (NoArg Extension)
            "Don't preserve file extensions"
    ,Option [] ["dry", "dry-run"] (NoArg Dry)
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
        return (nub args, concat fs)
    (_, _, errs) -> failWith $ concat errs ++ "\n" ++ usage
    where 
        exitWith msg = hPutStrLn stderr msg >> exit
        failWith msg = hPutStrLn stderr msg >> die
        header = "Usage: rename -vhe --dry-run [-d directory] [format string]"
        version = "rename version 1.5"
        usage = usageInfo header flags

listFilenames :: [Flag] -> IO [[FilePath]]
listFilenames flags = mapM (liftM sort . files) dirs
    where
        files dir = filterM doesFileExist =<< (liftM (dir </>)) <$> listDirectory dir
        dirs = if null flag_dirs then ["."] else flag_dirs
        flag_dirs = [dir | Directory dir <- flags]

buildTransformer :: [Flag] -> String -> IO (FileInfo -> FilePath)
buildTransformer flags format = do
    fun <- case parseFormat format of
        Left err -> print err >> die
        Right fmts -> getTransformer fmts
    
    return $ if (Extension `elem` flags)
            then dropExtension . fun
            else fun

buildRenamer :: [Flag] -> (FilePath -> FilePath -> IO ())
buildRenamer flags
    | Dry `elem` flags = log
    | otherwise = \old new -> do
        guard =<< not <$> doesPathExist new
        log old new
        renameFile old new
    where
        log old new = putStrLn $ old ++ " -> " ++ new
