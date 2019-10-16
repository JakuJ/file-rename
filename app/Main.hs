module Main where

import System.Environment (getArgs)
import System.Directory (listDirectory)
import Data.List (sort)

import ParseArgs (buildTransformer)
import Rename (newFilepaths, renameFiles)

main :: IO ()
main = do
    files <- sort <$> listDirectory "."
    transformer <- getArgs >>= buildTransformer
    renameFiles $ zip files $ newFilepaths files transformer
