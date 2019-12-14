module Main where

import Data.List (lines)
import SeatSorter
import System.Directory
import System.IO


main :: IO ()
main = do
  currentDirectory <- getCurrentDirectory
  handle <- openFile (currentDirectory ++ "/inputs/sample1.txt") ReadMode
  file <- hGetContents handle
  print $ lines file
  hClose handle

