module Day13 where

import qualified Data.Matrix as M
import Debug.Trace
import Fabs.LazyStdin (runOnInput)

data Memory
  = Irrelevant
  | LeftGo
  | StraightGo
  | RightGo

-- you forgot to consider to remember the directions of the crossings
star1 :: IO ()
star1 = print =<< runOnInput (solve . M.fromLists . mapMemory)

solve :: M.Matrix (Char, Memory) -> (Int, Int)
solve m
  | has 'X' m = coord 'X' m
  | otherwise = solve . iter $ m

iter :: M.Matrix (Char, Memory) -> M.Matrix (Char, Memory)
iter m =
  foldr
    decide
    (M.matrix (M.nrows m) (M.ncols m) (const ('.', Irrelevant)))
    (elems m)

-- fix indexes
-- two level matrix
decide ::
     (Int, Int, (Char, Memory))
  -> M.Matrix (Char, Memory)
  -> M.Matrix (Char, Memory)
decide (i, j, ('>', me)) m =
  M.setElem (move '>' (fst . M.getElem (i) (j) $ m) me) (i, j) m
decide (i, j, ('<', me)) m =
  M.setElem (move '<' (fst . M.getElem (i) (j) $ m) me) (i, j) m
decide (i, j, ('v', me)) m =
  M.setElem (move 'v' (fst . M.getElem (i) (j) $ m) me) (i, j) m
decide (i, j, ('^', me)) m =
  M.setElem (move '^' (fst . M.getElem (i) (j) $ m) me) (i, j) m
decide (i, j, c) m = M.setElem c (i, j) m

move :: Char -> Char -> Memory -> (Char, Memory)
move '>' '/' l = ('^', l)
move '>' '\\' l = ('v', l)
move '>' '+' RightGo = ('v', LeftGo)
move '>' '+' LeftGo = ('^', StraightGo)
move '<' '/' l = ('v', l)
move '<' '\\' l = ('^', l)
move '<' '+' RightGo = ('^', LeftGo)
move '<' '+' LeftGo = ('v', StraightGo)
move 'v' '/' l = ('<', l)
move 'v' '\\' l = ('>', l)
move 'v' '+' RightGo = ('<', LeftGo)
move 'v' '+' LeftGo = ('>', StraightGo)
move '^' '/' l = ('>', l)
move '^' '\\' l = ('<', l)
move '^' '+' RightGo = ('>', LeftGo)
move '^' '+' LeftGo = ('<', StraightGo)
move c '+' StraightGo = (c, RightGo)

nextGo LeftGo = StraightGo
nextGo StraightGo = RightGo
nextGo RightGo = LeftGo

has :: Char -> M.Matrix (Char, Memory) -> Bool
has c m = any (\(_, _, (k, _)) -> k == c) $ elems m

coord :: Char -> M.Matrix (Char, Memory) -> (Int, Int)
coord c m =
  head . fmap (\(i, j, _) -> (i, j)) . filter (\(_, _, (k, _)) -> k == c) $
  elems m

elems :: M.Matrix (Char, Memory) -> [(Int, Int, (Char, Memory))]
elems m =
  [(i, j, M.getElem i j m) | i <- [1 .. (M.nrows m)], j <- [1 .. (M.ncols m)]]

mapMemory :: [String] -> [[(Char, Memory)]]
mapMemory ls = fmap (fmap bld) ls
  where
    bld '>' = ('>', LeftGo)
    bld '<' = ('<', LeftGo)
    bld 'v' = ('v', LeftGo)
    bld '^' = ('^', LeftGo)
    bld c = (c, Irrelevant)
