module Main where

import System.Environment (getArgs)
import System.Directory (listDirectory)
import Data.List (sort)

import ParseArgs (parseArgs, buildTransformer, buildRenamer)
import Rename (newFilepaths, renameWith)

main :: IO ()
main = do
    files <- sort <$> listDirectory "."
    (flags, format) <- getArgs >>= parseArgs
    transformer <- buildTransformer flags format
    let renamer = buildRenamer flags
    renameWith renamer $ zip files $ newFilepaths files transformer
