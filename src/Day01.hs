{-# LANGUAGE OverloadedStrings #-}

module Day01
  ( star1
  , star2
  ) where

import Fabs.IntSetWithLast as Set
import Fabs.LazyStdin (runOnInput)

star1 :: IO ()
star1 = print =<< runOnInput sum
  where
    sum = foldr addIntString 0

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
        freq = addIntString n (safeLast 0 s)
        nextSearch = insert s freq

addIntString :: String -> Int -> Int
addIntString n sum = (+ sum) $ readSignaledInt n

readSignaledInt :: String -> Int
readSignaledInt ('+':cs) = readInt cs
readSignaledInt ('-':cs) = -1 * readInt cs
readSignaledInt s@(_:cs) = error $ "Malformed string" ++ s

readInt = read :: (String -> Int)
