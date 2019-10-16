module Rename (
    newFilepaths,
    renameWith
) where

import System.FilePath.Posix (takeFileName, takeExtension)

import Format (FileInfo (FileInfo))

mkFileInfo :: FilePath -> Int -> FileInfo
mkFileInfo fp = FileInfo (takeFileName fp) (takeExtension fp)

newFilepaths :: [FilePath] -> (FileInfo -> FilePath) -> [FilePath]
newFilepaths files = flip map infos
    where
        infos = zipWith mkFileInfo files [1..]

renameWith :: (FilePath -> FilePath -> IO ()) -> [(FilePath, FilePath)] -> IO ()
renameWith renamer = mapM_ $ uncurry renamer
