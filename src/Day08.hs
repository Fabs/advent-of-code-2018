module Day08 where

import Data.List.Split
import Debug.Trace
import Fabs.LazyStdin (runOnInput)

data Node = Node
  { id :: (Int, Int)
  , meta :: [Int]
  , child :: [Node]
  , value :: Int
  } deriving (Eq, Show)

star1 :: IO ()
star1 =
  print =<< runOnInput ((sumMetadata 0) . snd . listNodes . intList . head)
  where
    sumMetadata :: Int -> [Node] -> Int
    sumMetadata acc [] = acc
    sumMetadata acc (n:ns) = (sum $ meta n) + sumMetadata acc (ns ++ child n)
    listNodes ns = parseNodes ([1, 0] ++ ns) []
    intList = fmap (read :: String -> Int) . splitOn " "

parseNodes :: [Int] -> [Node] -> ([Int], [Node])
parseNodes [] ns = ([], ns)
parseNodes (0:m:r) ns = metadata m r ns
parseNodes (ch:m:r) ns = (thisRest, ns ++ chs)
  where
    (thisRest, chs) = parseNodesTimes ch r []
    parseNodesTimes 0 r' chs' = metadata m r' chs'
    parseNodesTimes ck r' chs' = parseNodesTimes (ck - 1) nextR nextChs
      where
        (nextR, nextChs) = parseNodes r' chs'
parseNodes is ns = traceShow (is, ns) error "botom should not happen"

metadata :: Int -> [Int] -> [Node] -> ([Int], [Node])
metadata m r chs = (drop m r, [Node (0, m) meta' chs (valueCalc meta' chs)])
  where
    meta' = take m r
    valueCalc m' [] = sum m'
    valueCalc m' chs' = sum . fmap (elemSafe chs') $ m'
    elemSafe ls i
      | (i - 1) < (length ls) = value (ls !! (i - 1))
      | otherwise = 0

star2 :: IO ()
star2 = print =<< runOnInput (rootValue . snd . listNodes . intList . head)
    -- traversable anyone :[
  where
    rootValue (d:_) = value d
    listNodes ns = parseNodes ([1, 1] ++ ns ++ [1]) []
    intList = fmap (read :: String -> Int) . splitOn " "
