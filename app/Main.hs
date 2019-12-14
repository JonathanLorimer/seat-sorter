module Main where

import Data.List (lines)
import Data.Maybe
import Data.Text (pack, strip)
import Text.Megaparsec (parseMaybe)
import SeatSorter
import System.Directory
import System.IO


main :: IO ()
main = do
  currentDirectory <- getCurrentDirectory
  handle <- openFile (currentDirectory ++ "/inputs/sample1.txt") ReadMode
  file <- hGetContents handle
  let input = strip . pack <$> drop 1 (lines file)
  let parsedTickets = catMaybes $ parseMaybe ticketParser <$> input
  let ticketMap = groupTickets parsedTickets
  print $ joinNonConsecutive . permuteConsecutive . sequences <$> ticketMap
  hClose handle

