module Day03 where

-- Oh I wish I had time to refactor you :/
import qualified Data.IntSet as S
  ( IntSet
  , difference
  , empty
  , insert
  , member
  , size
  )
import Data.Ix (range)
import Data.Maybe (isJust)
import Fabs.LazyStdin (runOnInput)
import Text.Regex.Posix

data Rect =
  Rect Int
       (Int, Int)
       (Int, Int)
  deriving (Show)

-- the match looks like:
-- [["#1 @ 1,3: 4x4","1","3","4","4"]]
-- so we need to get the first, than drop the full match, hence
-- tail . head
-- I would love to have done that with trifecta
squareRegex :: String -> [Int]
squareRegex s = fmap (read :: String -> Int) $ tail . head . match $ s
  where
    match :: String -> [[String]]
    match s =
      ((s :: String) =~ "#([0-9]+) @ ([0-9]+),([0-9]+): ([0-9]+)x([0-9]+)" :: [[String]])

parseRects :: [String] -> [Rect]
parseRects = fmap $ parseRect . squareRegex

parseRect :: [Int] -> Rect
parseRect (id:x:y:l:h:[]) = Rect id (x, y) (x + l - 1, y + h - 1)
parseRect _ = error "does not look like a square"

star1 :: IO ()
star1 = print =<< runOnInput countOverlaps

star2 :: IO ()
star2 = print =<< runOnInput findNonOverlapped

findNonOverlapped :: [String] -> [Maybe Rect]
findNonOverlapped rs = filter isJust findNonRepeat
  where
    (_, rps) = traverseRects rs
    allRectPieces = parseRects rs
    findNonRepeat = fmap find (allRectPieces)
      where
        find :: Rect -> Maybe Rect
        find r@(Rect id _ _)
          | nonOverlaps == (length pieces) = Just r
          | otherwise = Nothing
          where
            pieces = allPieces r
            nonOverlaps = length $ filter (\n -> not $ S.member n rps) pieces

countOverlaps :: [String] -> Int
countOverlaps = S.size . snd . traverseRects

traverseRects :: [String] -> (S.IntSet, S.IntSet)
traverseRects = foldr findOverlap (S.empty, S.empty) . parseRects

findOverlap :: Rect -> (S.IntSet, S.IntSet) -> (S.IntSet, S.IntSet)
findOverlap r (s, c) = foldr findMatch (s, c) (allPieces r)

findMatch :: Int -> (S.IntSet, S.IntSet) -> (S.IntSet, S.IntSet)
findMatch n (s, c)
  | S.member n s = (s, S.insert n c)
  | otherwise = (S.insert n s, c)

allPieces :: Rect -> [Int]
allPieces (Rect _ s t) = fmap pieceId (range (s, t))

pieceId :: (Int, Int) -> Int
pieceId (x, y) = y + 10000 * x
