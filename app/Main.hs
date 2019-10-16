module Main where

import System.Environment (getArgs)

import ParseArgs (parseArgs, listFilenames, buildTransformer, buildRenamer)
import Rename (newFilepaths, renameWith)

main :: IO ()
main = do
    (flags, format) <- getArgs >>= parseArgs
    files <- listFilenames flags
    transformer <- buildTransformer flags format
    let renamer = buildRenamer flags
    let namePairs = zip files $ newFilepaths files transformer
    renameWith renamer namePairs
