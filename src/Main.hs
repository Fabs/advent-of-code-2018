module Main where

import qualified Day01
import qualified Day02
import System.Environment (getArgs)

runFunc :: [String] -> IO ()
runFunc ["1", "1"] = Day01.star1
runFunc ["1", "2"] = Day01.star2
runFunc ["2", "1"] = Day02.star1
runFunc ["2", "2"] = Day02.star2

main :: IO ()
main = runFunc =<< getArgs

runOnInput :: ([String] -> a) -> IO a
runOnInput f = f . lines <$> getContents

readInteger = read :: (String -> Integer)

readSignaledInt :: String -> Integer
readSignaledInt ('+':cs) = readInteger cs
readSignaledInt ('-':cs) = -1 * readInteger cs
readSignaledInt s@(_:cs) = error $ "Malformed string" ++ s
