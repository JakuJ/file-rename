module ParseArgs (
    parseArgs,
    listFilenames,
    buildTransformer,
    buildRenamer
) where

import System.IO (hPutStrLn, stderr)
import System.Console.GetOpt (OptDescr (..), ArgDescr (..), ArgOrder (..), getOpt, usageInfo)
import System.Exit (exitWith, ExitCode (..))
import System.Directory (renamePath, listDirectory, doesPathExist, doesFileExist)
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
    | Extension | AllowDirs | DryRun | Version | Help
    deriving (Eq, Show)

flags :: [OptDescr Flag]
flags =
    [Option ['d'] ["dir"] (ReqArg Directory "DIR")
            "Directory to rename files in - defaults to the current one"
    ,Option ['e'] [] (NoArg Extension)
            "Don't preserve file extensions"
    ,Option [] ["allow-dirs"] (NoArg AllowDirs)
            "Also rename directories"
    ,Option [] ["dry", "dry-run"] (NoArg DryRun)
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
        version = "rename version 1.6"
        usage = usageInfo header flags

listFilenames :: [Flag] -> IO [[FilePath]]
listFilenames flags = mapM (liftM sort . files) dirs
    where
        files dir = filterM pathFilter =<< (liftM (dir </>)) <$> listDirectory dir
        pathFilter = if AllowDirs `elem` flags then doesPathExist else doesFileExist
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
    | DryRun `elem` flags = log
    | otherwise = \old new -> do
        guard =<< not <$> doesPathExist new
        log old new
        renamePath old new
    where
        log old new = putStrLn $ old ++ " -> " ++ new
