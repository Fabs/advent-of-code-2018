module Day14 where

import qualified Data.Vector as V
import Debug.Trace

star1 :: Int -> Int -> IO ()
star1 p x = print (x, solve p 0 (V.fromList [3, 7]) (0, 1))

solve :: Int -> Int -> V.Vector Int -> (Int, Int) -> ((Int, Int), V.Vector Int)
solve p s ls e@(e1, e2)
  | p + 11 < V.length ls = (e, V.take 10 . V.drop p $ ls)
  | otherwise = solve p s ls ne
  where
    ve1 = (ls V.! e1)
    ve2 = (ls V.! e2)
    ie1 = 1 + ve1 + e1
    ie2 = 1 + ve2 + e2
    nnums = nums (ve1 + ve2)
    new = length ls + length nnums
    ne = ord' ((ie1 `mod` new), (ie2 `mod` new))
    -- (p', l', e', s') = shorten s p ((V.++) ls nnums) ne

ord' (a, b)
  | a < b = (a, b)
  | otherwise = (b, a)

nums x
  | x <= 9 = V.singleton x
  | otherwise = V.fromList [1, x `mod` 10]
