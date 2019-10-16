module Rename (Format(..), FileInfo, identifiers, getTransform, renameFiles) where

import System.Directory (listDirectory, renameFile)
import System.Posix.Files (getFileStatus, fileSize)
import System.FilePath.Posix (takeFileName, takeExtension)
import Data.Time (utctDay, getCurrentTime)

data FileInfo = FileInfo {name :: String, extension :: String, number :: Int}

mkFileInfo :: FilePath -> Int -> FileInfo
mkFileInfo fp = FileInfo (takeFileName fp) (takeExtension fp)

renameFiles :: (FileInfo -> FilePath) -> IO ()
renameFiles f = do
    files <- listDirectory "."
    let infos = zipWith mkFileInfo files [1..]
    let newFiles = map f infos
    mapM_ (uncurry renameFile) $ zip files newFiles
    
-- parsing special identifiers

data Format = Special String | Const String
    deriving (Show)

identifiers :: [String]
identifiers = ["name", "number", "date", "size"]

getFun :: Format -> IO (FileInfo -> FilePath)
getFun (Const str) = return $ const str
getFun (Special "name") = return name
getFun (Special "number") = return $ show . number
getFun (Special "date") = const <$> show . utctDay <$> getCurrentTime

getTransform :: [Format] -> IO (FileInfo -> FilePath)
getTransform formats = do
    funs <- mapM getFun formats
    return $ \fi -> concatMap ($ fi) funs ++ extension fi