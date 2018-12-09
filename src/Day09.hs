module Day09 where

import Debug.Trace
import Fabs.LazyStdin (runOnInput)

-- optmize this later with maybe
-- import Data.List.PointedList.Circular
star1 :: Int -> Int -> Int -> IO ()
star1 p m e = print (e, game p m)

star2 :: Int -> Int -> IO ()
star2 _ _ = print =<< runOnInput id

game p m = maximum . V.toList $ gameStep marbles p 0 startScore 1 board head'
  where
    startScore = V.replicate (p + 1) 0
    board = V.replicate m 0
    head' = 0
    marbles = [1 .. m]

gameStep ::
     [Int]
  -> Int
  -> Int
  -> V.Vector Int
  -> Int
  -> V.Vector Int
  -> Int
  -> V.Vector Int
gameStep [] _ _ s _ _ _ = s
gameStep (m:ms) p cp s l b h
  | m `mod` 23 == 0 = specialPlacement (m : ms) p cp s l b h
  | otherwise = normalPlacement (m : ms) p cp s l b h

specialPlacement ::
     [Int]
  -> Int
  -> Int
  -> V.Vector Int
  -> Int
  -> V.Vector Int
  -> Int
  -> V.Vector Int
specialPlacement (m:ms) p cp s l b h = gameStep ms p np score (l - 1) board lb
  where
    addScore = (V.!) s cp + rM + m
    score = (V.//) s [(cp, addScore)]
    board = V.filter (/= rM) b
    lb = cycleIdx (h - 7) l
    r1 = cycleIdx (lb + 1) l
    rM = (V.!) b lb
    np = traceShow m $ (cp + 1) `mod` p

normalPlacement ::
     [Int]
  -> Int
  -> Int
  -> V.Vector Int
  -> Int
  -> V.Vector Int
  -> Int
  -> V.Vector Int
normalPlacement (m:ms) p cp s l b h = gameStep ms p np s (l + 1) board lb
  where
    board = (V.slice 0 lb b) V.++ (V.singleton m) V.++ (V.slice lb (l - lb) b)
    lb = cycleIdx (h + 2) l
    np = traceShow $ (cp + 1) `mod` p

cycleIdx i l = (l + i `mod` l) `mod` l
