module Format (
    Format (..),
    FileInfo (..),
    identifiers,
    getTransformer
) where

import System.Posix.Files (getFileStatus, fileSize)
import Data.Time (utctDay, getCurrentTime)

data Format = Special String | Const String
    deriving (Show)

identifiers :: [String]
identifiers = ["name", "number", "date"]

data FileInfo = FileInfo {name :: String, extension :: String, number :: Int}

getFun :: Format -> IO (FileInfo -> FilePath)
getFun (Const str) = return $ const str
getFun (Special "name") = return name
getFun (Special "number") = return $ show . number
getFun (Special "date") = const <$> show . utctDay <$> getCurrentTime

getTransformer :: [Format] -> IO (FileInfo -> FilePath)
getTransformer formats = do
    funs <- mapM getFun formats
    return $ \fi -> concatMap ($ fi) funs