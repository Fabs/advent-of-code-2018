module Day06 where

import Data.Char
import Data.List ((\\), nub, sort)
import qualified Data.Map.Strict as Map
import Data.Matrix
import Fabs.LazyStdin (runOnInput)
import Text.Regex.Posix

data Loc =
  Loc Char
      (Int, Int)
  deriving (Show, Eq, Ord)

filterIndexed :: (a -> Int -> Bool) -> [a] -> [a]
filterIndexed p xs = [x | (x, i) <- zip xs [0 ..], p x i]

locRegex :: String -> [Int]
locRegex s = fmap (read :: String -> Int) $ tail . head . match $ s
  where
    match :: String -> [[String]]
    match s = ((s :: String) =~ "([0-9]+), ([0-9]+)" :: [[String]])

locName :: Int -> Char
locName n = chr $ (ord 'a') + n

star1 :: IO ()
star1 =
  print =<<
  runOnInput (\ls -> walk (objective (start ls)) 0 max Map.empty (start ls))
  where
    max = 100
    dim = 14 :: Int
    objective ls w lend = sort $ fmap (\c -> area w c) (allTiles \\ infinite)
      where
        infinite = nub $ (fmap (\(Loc c _) -> c)) $ lend
        allTiles = fmap (\(Loc c _) -> c) ls
        area w c =
          foldr (\(_, _) (t, c) -> (t + 1, c)) (0, c) $
          filter (\(k, v) -> v == c) $ Map.toList w
    makeMatrix w =
      foldr (\((a, b), c) m -> setElem c (a, b) m) (matrix dim dim (const '_')) $
      filter (\((a, b), c) -> a > 0 && b > 0) (Map.toList w)

-- step max_step walks available search transform
walk ::
     (Map.Map (Int, Int) Char -> [Loc] -> b)
  -> Int
  -> Int
  -> Map.Map (Int, Int) Char
  -> [Loc]
  -> b
walk f k kmax w ls
  | kmax == k = f w ls
  | otherwise = walk f (k + 1) kmax wNext lsNext
  where
    lsNext =
      map (\((a, b), c) -> Loc c (a, b)) $
      filter (\((a, b), c) -> not (Map.member (a, b) wStep)) $
      Map.toList candidateSet
    wStep = foldr (\(Loc c (a, b)) w -> Map.insert (a, b) c w) w ls
    wNext = wStep
    candidateSet =
      foldr insertType (Map.empty :: Map.Map (Int, Int) Char) allCandidates
      where
        insertType :: Loc -> Map.Map (Int, Int) Char -> Map.Map (Int, Int) Char
        insertType loc@(Loc c (a, b)) cs
          | Map.member (a, b) cs = Map.update (updateDiff c) (a, b) cs
          | otherwise = Map.insert (a, b) c cs
    updateDiff c c'
      | c == c' = Just c
      | otherwise = Just '.'
    allCandidates = foldr (\c cs -> (candidate c) ++ cs) [] ls
    candidate l =
      filter (\(Loc c (a, b)) -> not $ Map.member (a, b) wStep) $ manhattan l
    manhattan (Loc c (a, b)) =
      [Loc c (a + 1, b), Loc c (a, b + 1), Loc c (a - 1, b), Loc c (a, b - 1)]

star2 :: IO ()
star2 = print =<< runOnInput (length . safeArea . start)
  where
    safeArea ds = filter (bellow ds 10000) manyCoordinates
    bellow ds n loc = (< n) $ foldr (distance loc) 0 ds
      where
        distance (a', b') (Loc _ (a, b)) s = s + (abs (a - a')) + (abs (b - b'))
    manyCoordinates = [(x, y) | x <- [0 .. 5000], y <- [0 .. 5000]]

start ls = group (0 :: Int) $ fmap locRegex ls
  where
    group i [] = []
    group i ((a:b:[]):ls) = Loc (locName i) (b, a) : group (i + 1) ls
