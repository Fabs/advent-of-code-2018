module Day09 where

import Data.List.PointedList.Circular as R
import Data.Vector as V
import Debug.Trace
import Fabs.LazyStdin (runOnInput)

-- optmize this later with maybe
-- import Data.List.PointedList.Circular
star1 :: Int -> Int -> Int -> IO ()
star1 p m e = print (e, game p m)

star2 :: Int -> Int -> IO ()
star2 _ _ = print =<< runOnInput id

game p m = V.maximum $ gameStep marbles p 0 startScore board
  where
    startScore = V.replicate (p + 1) 0
    board = R.singleton 0
    head' = 0
    marbles = [1 .. m]

gameStep ::
     [Int] -> Int -> Int -> V.Vector Int -> R.PointedList Int -> V.Vector Int
gameStep [] _ _ s _ = s
gameStep (m:ms) p cp s r
  | m `mod` 23 == 0 = specialPlacement (m : ms) p cp s r
  | otherwise = normalPlacement (m : ms) p cp s r

specialPlacement ::
     [Int] -> Int -> Int -> V.Vector Int -> R.PointedList Int -> V.Vector Int
specialPlacement (m:ms) p cp s r = gameStep ms p np score nr
  where
    addScore = (V.!) s cp + rM + m
    score = (V.//) s [(cp, addScore)]
    rM = _focus . R.moveN (-8) $ r
    nr = R.next . (maybe (R.singleton 0) id) . R.delete . R.moveN (-8) $ r
    np = (cp + 1) `mod` p

normalPlacement ::
     [Int] -> Int -> Int -> V.Vector Int -> R.PointedList Int -> V.Vector Int
normalPlacement (m:ms) p cp s r = gameStep ms p np s nr
  where
    nr = R.next . R.insert m $ r
    np = (cp + 1) `mod` p
