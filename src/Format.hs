module Format (
    Format (..),
    FileInfo (..),
    identifiers,
    getTransformer
) where

import System.Posix.Files (getFileStatus, fileSize)
import System.FilePath.Posix (replaceBaseName, takeFileName)
import Data.Time (utctDay, getCurrentTime)

data Format = Special String | Const String
    deriving (Show)

identifiers :: [String]
identifiers = ["name", "number", "date"]

data FileInfo = FileInfo {path :: FilePath, number :: Int}

getFun :: Format -> IO (FileInfo -> String)
getFun (Const str) = return $ const str
getFun (Special "name") = return $ takeFileName . path
getFun (Special "number") = return $ show . number
getFun (Special "date") = const <$> show . utctDay <$> getCurrentTime

getTransformer :: [Format] -> IO (FileInfo -> FilePath)
getTransformer formats = do
    funs <- mapM getFun formats
    return $ \fi -> replaceBaseName (path fi) $ concatMap ($ fi) funs