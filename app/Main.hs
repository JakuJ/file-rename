module Main where

import System.Environment (getArgs)
import Control.Monad (forM_)

import ParseArgs (parseArgs, listFilenames, buildTransformer, buildRenamer)
import Rename (newFilepaths, renameWith)

main :: IO ()
main = do
    (flags, format) <- getArgs >>= parseArgs
    fileLists <- listFilenames flags
    transformer <- buildTransformer flags format
    let renamer = buildRenamer flags
    forM_ fileLists $ \(dir, files) -> do
        putStrLn $ "Processing directory: " ++ dir
        let namePairs = zip files $ newFilepaths files transformer
        renameWith renamer namePairs
