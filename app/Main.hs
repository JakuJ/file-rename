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
    forM_ fileLists $ \fullPaths -> do
        let namePairs = zip fullPaths $ newFilepaths fullPaths transformer
        renameWith renamer namePairs
