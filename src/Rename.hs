module Rename (Format(..), identifiers, getTransform, renameFiles) where

import System.Posix.Files
import System.Directory (listDirectory, renameFile)
import Data.Time

data Format = Special String | Const String
    deriving (Show)

-- parsing special identifiers

identifiers :: [String]
identifiers = ["name", "number", "date", "size"]

-- the rename function
renameFiles :: (FilePath -> FilePath) -> IO ()
renameFiles f = do
    files <- listDirectory "."
    let newFiles = map f files
    mapM_ (uncurry renameFile) $ zip files newFiles

-- format to transformation function

getFun :: Format -> IO (FilePath -> FilePath)
getFun (Const str) = return $ const str
getFun (Special "name") = return id
getFun (Special "number") = return $ const "_num_" -- !
getFun (Special "date") = const <$> show . utctDay <$> getCurrentTime
-- getFun (Special "size") = (fileSize <$> getFileStatus)

getTransform :: [Format] -> IO (FilePath -> FilePath)
getTransform formats = do
    funs <- mapM getFun formats
    return $ \fp -> concatMap ($ fp) funs