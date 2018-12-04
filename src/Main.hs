module Main where

import qualified Day01
import qualified Day02
import qualified Day03
import System.Environment (getArgs)

runFunc :: [String] -> IO ()
runFunc ["1", "1"] = Day01.star1
runFunc ["1", "2"] = Day01.star2
runFunc ["2", "1"] = Day02.star1
runFunc ["2", "2"] = Day02.star2
runFunc ["3", "1"] = Day03.star1
runFunc ["3", "2"] = Day03.star2

main :: IO ()
main = runFunc =<< getArgs
