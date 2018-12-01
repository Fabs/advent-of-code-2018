{-# LANGUAGE OverloadedStrings #-}

module Day01
  ( star1
  , star2
  ) where

import qualified Data.IntSet as S (IntSet, empty, insert, member, size)
import Data.Maybe
import LazyStdin (runOnInput)

star1 :: IO ()
star1 = print =<< runOnInput sum
  where
    sum = foldr addIntString 0

data Set = Set
  { set :: S.IntSet
  , lastIns :: Maybe Int
  }

member :: Set -> Int -> Bool
member s r = S.member r (set s)

insert :: Set -> Int -> Set
insert (Set set _) s = Set (S.insert s set) (Just s)

empty :: Set
empty = Set S.empty Nothing

star2 :: IO ()
star2 = print =<< runOnInput find
  where
    find :: [String] -> Int
    find = searchRepeated empty . cycle
    searchRepeated :: Set -> [String] -> Int
    searchRepeated s (n:ns)
      | member s freq = freq
      | otherwise = searchRepeated nextSearch ns
      where
        safeLast = fromMaybe 0 . lastIns
        freq = addIntString n (safeLast s)
        nextSearch = insert s freq

addIntString :: String -> Int -> Int
addIntString n sum = (+ sum) $ readSignaledInt n

readSignaledInt :: String -> Int
readSignaledInt ('+':cs) = readInt cs
readSignaledInt ('-':cs) = -1 * readInt cs
readSignaledInt s@(_:cs) = error $ "Malformed string" ++ s

readInt = read :: (String -> Int)
