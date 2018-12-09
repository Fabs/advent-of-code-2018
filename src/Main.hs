module Main where

import qualified Day01
import qualified Day02
import qualified Day03
import qualified Day04
import qualified Day05
import qualified Day06
import qualified Day07
import qualified Day08
import qualified Day09
import System.Environment (getArgs)

runFunc :: [String] -> IO ()
runFunc ["1", "1"] = Day01.star1
runFunc ["1", "2"] = Day01.star2
runFunc ["2", "1"] = Day02.star1
runFunc ["2", "2"] = Day02.star2
runFunc ["3", "1"] = Day03.star1
runFunc ["3", "2"] = Day03.star2
runFunc ["4", "1"] = Day04.star1
runFunc ["4", "2"] = Day04.star2
runFunc ["5", "1"] = Day05.star1
runFunc ["5", "2"] = Day05.star2
runFunc ["6", "1"] = Day06.star1
runFunc ["6", "2"] = Day06.star2
runFunc ["7", "1"] = Day07.star1
runFunc ["7", "2"] = Day07.star2
runFunc ["8", "1"] = Day08.star1
runFunc ["8", "2"] = Day08.star2
runFunc ["9", "1", pls, pts, expt] =
  Day09.star1 (toInt pls) (toInt pts) (toInt expt)

toInt = read :: String -> Int

main :: IO ()
main = runFunc =<< getArgs
