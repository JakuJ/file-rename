module Rename (
    renameFiles
) where

import System.Directory (listDirectory, renameFile)
import System.FilePath.Posix (takeFileName, takeExtension)

import Format (FileInfo (FileInfo))

mkFileInfo :: FilePath -> Int -> FileInfo
mkFileInfo fp = FileInfo (takeFileName fp) (takeExtension fp)

renameFiles :: (FileInfo -> FilePath) -> IO ()
renameFiles f = do
    files <- listDirectory "."
    let infos = zipWith mkFileInfo files [1..]
    let newFiles = map f infos
    mapM_ (uncurry renameFile) $ zip files newFiles
