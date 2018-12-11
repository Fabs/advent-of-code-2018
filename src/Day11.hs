module Day11 where

import Data.Map as H
import Data.Matrix as M
import Debug.Trace
import Fabs.LazyStdin (runOnInput)

star1 :: Int -> IO ()
star1 s = print $ (mi, mj, mz, c)
  where
    (mz, mi, mj, c, _) =
      Prelude.foldr
        findMax
        (0, 0, 0, 0, H.empty :: Map (Int, Int, Int) Int)
        [ (z, x, y)
        | z <- reverse [1 .. 300]
        , x <- [1 .. (300 - z - 1)]
        , y <- [1 .. (300 - z - 1)]
        ]
      where
        matrix = (ma s)
        findMax (z, i, j) (zmax, xmax, ymax, max, h)
          | c > max = (z, i, j, c, nh)
          | otherwise = (zmax, xmax, ymax, max, nh)
          where
            (c, nh) = sumMatrix matrix h z i j

sumMatrix m h z i j =
  case H.member (z, i, j) h of
    True -> ((H.!) h (z, i, j), h)
    False -> (su m h z i j, H.insert (z, i, j) (su m h z i j) h)

su m h 1 i j = M.getElem i j m
su m h 2 i j = Prelude.foldr (+) 0 $ submatrix i (i + 1) j (j + 1) m
su m h k i j =
  ((H.!) h (k - 1, i + 1, j)) + ((H.!) h (k - 1, i, j + 1)) -
  ((H.!) h (k - 2, i + 1, j + 1)) +
  M.getElem (i + k - 1) (j + k - 1) m +
  M.getElem i j m

starQ :: Int -> Int -> Int -> IO ()
starQ s i j = print (queryM s i j)

solve s _ = (s, 0)

queryM s x y = M.getElem x y (ma s)

ma s = M.matrix 300 300 (fuel s)

fuel s (i, j) = level
  where
    rackId = i + 10
    powerLevel = rackId * j
    serialPower = powerLevel + s
    rackSerialPower = serialPower * rackId
    digit = (rackSerialPower `div` 100) `mod` 10
    level = digit - 5
