module Day15 where

import Fabs.LazyStdin (runOnInput)
import Text.Regex.Posix

data Parsed
  = After
  | Before
  | Cmd

matchAny s = [matchAft, matchBefore, matchCmd] <*> [s]

matchAft s =
  ( (s :: String) =~~ "Before: \\[([0-9]+),([0-9]+),([0-9]+),([0-9]+)\\]" :: Maybe [[String]]
  , After)

matchBefore s =
  ( (s :: String) =~~ "After:  \\[([0-9]+),([0-9]+),([0-9]+),([0-9]+)\\]" :: Maybe [[String]]
  , After)

matchCmd s =
  ( (s :: String) =~~ "([0-9]+) ([0-9]+) ([0-9]+) ([0-9]+)" :: Maybe [[String]]
  , After)

star1 :: IO ()
star1 = print =<< runOnInput test

test = undefined
