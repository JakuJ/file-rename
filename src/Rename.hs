module Rename (
    newFilepaths,
    renameWith
) where

import Format (FileInfo (FileInfo))

newFilepaths :: [FilePath] -> (FileInfo -> FilePath) -> [FilePath]
newFilepaths files = flip map infos
    where
        infos = zipWith FileInfo files [1..]

renameWith :: (FilePath -> FilePath -> IO ()) -> [(FilePath, FilePath)] -> IO ()
renameWith renamer = mapM_ $ uncurry renamer
