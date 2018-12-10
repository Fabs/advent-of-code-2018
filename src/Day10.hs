module Day10 where

import Data.List (minimumBy)
import Data.Matrix as M
import Debug.Trace
import Fabs.LazyStdin (runOnInput)
import Text.Regex.Posix

data Particle = Particle
  { x :: Int
  , y :: Int
  , vx :: Int
  , vy :: Int
  } deriving (Show)

particleRegex :: String -> [Int]
particleRegex s = fmap (read :: String -> Int) $ tail . head . matches $ s
  where
    matches :: String -> [[String]]
    matches s' =
      (s' :: String) =~
      "position=<[ ]*([0-9\\-]+),[ ]*([0-9\\-]+)> velocity=<[ ]*([0-9\\-]+),[ ]*([0-9\\-]+)>" :: [[String]]

particle :: [Int] -> Particle
particle (x':y':vx':vy':[]) = Particle y' x' vy' vx'
particle _ = error "can't parse particle"

star1 :: Int -> IO ()
star1 t = print =<< runOnInput solve
  where
    solve = (simulate t . fmap (particle . particleRegex))

star2 :: IO ()
star2 = print =<< runOnInput solve
  where
    solve ps = (mint, simulate mint parse)
      where
        parse = fmap (particle . particleRegex) ps
        (mint, _) =
          minimumBy compareDim $ fmap (\t -> (t, dim t parse)) [1 .. 100000]
        compareDim (_, (x, y)) (_, (x', y')) = compare (x * y) (x' * y')

dim :: Int -> [Particle] -> (Int, Int)
dim t ps = (abs (xmax - xmin) + 1, abs (ymax - ymin) + 1)
  where
    pos t' p = Particle (x p + t' * vx p) (y p + t' * vy p) (vx p) (vy p)
    pst = fmap (pos t) ps
    xmin = minimum . fmap x $ pst
    xmax = maximum . fmap x $ pst
    ymin = minimum . fmap y $ pst
    ymax = maximum . fmap y $ pst

simulate :: Int -> [Particle] -> M.Matrix Char
simulate t ps = foldr build board psProj
  where
    build p = M.setElem '#' (x p, y p)
    board = matrix xDim yDim (const '.')
    psProj = fmap (posX t) ps
    pst = fmap (pos t) ps
    posX t' p =
      Particle
        (x p + t' * vx p - xmin + 1)
        (y p + t' * vy p - ymin + 1)
        (vx p)
        (vy p)
    pos t' p = Particle (x p + t' * vx p) (y p + t' * vy p) (vx p) (vy p)
    xmin = minimum . fmap x $ pst
    xmax = maximum . fmap x $ pst
    ymin = minimum . fmap y $ pst
    ymax = maximum . fmap y $ pst
    (xDim, yDim) = (abs (xmax - xmin) + 1, abs (ymax - ymin) + 1)
