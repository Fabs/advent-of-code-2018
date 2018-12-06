module Day05 where

import Data.Char
import Fabs.LazyStdin (runOnInput)

star1 :: IO ()
star1 = print =<< runOnInput (react . head)

star2 :: IO ()
star2 = print =<< runOnInput (minimize . head)
  where
    minimize s = foldr (reactSize s) (Nothing, Nothing) ['a' .. 'z']
    reactSize s c m@(Nothing, Nothing) = (Just c, Just $ reaction c s)
    reactSize s c m@(_, Just min)
      | reaction c s > min = (Just c, Just $ reaction c s)
      | otherwise = m
    reaction c s = react $ filtered c s
    filtered c = filter (/= c) . filter (/= toUpper c)

react = length . removeAntipods ""

removeAntipods :: String -> String -> String
removeAntipods s "" = reverse s
removeAntipods "" (c:cs) = removeAntipods (c : "") cs
removeAntipods (s:ss) (c:cs)
  | antipods s c = removeAntipods ss cs
  | otherwise = removeAntipods (c : s : ss) cs

antipods c c' = (== 32) . abs $ ord c - ord c'
