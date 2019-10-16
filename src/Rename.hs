module Rename (
    newFilepaths,
    renameFiles
) where

import Control.Monad (forM_, guard)
import System.Posix.Files (fileExist)
import System.Directory (renameFile)
import System.FilePath.Posix (takeFileName, takeExtension)

import Format (FileInfo (FileInfo))

mkFileInfo :: FilePath -> Int -> FileInfo
mkFileInfo fp = FileInfo (takeFileName fp) (takeExtension fp)

newFilepaths :: [FilePath] -> (FileInfo -> FilePath) -> [FilePath]
newFilepaths files = flip map infos
    where
        infos = zipWith mkFileInfo files [1..]

renameFiles :: [(FilePath, FilePath)] -> IO ()
renameFiles pairs = forM_ pairs $ \(old, new) -> do
    guard =<< not <$> fileExist new
    putStrLn $ old ++ " -> " ++ new
    renameFile old new
