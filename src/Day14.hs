{-# LANGUAGE OverloadedStrings #-}

module Day14 where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Maybe
import Debug.Trace

star1 :: Int -> Int -> IO ()
star1 p x = print (x, solve p "910109" "37" (0, 1))

solve ::
     Int
  -> B.ByteString
  -> B.ByteString
  -> (Int, Int)
  -> ((Int, Int), B.ByteString, Int)
solve p s ls e@(e1, e2)
  | B.length lastMatch /= 0 = (e, lastMatch, B.length ls)
  | p + 11 < B.length ls = (e, B.take 10 . B.drop p $ ls, 0)
  | otherwise = solve p s nls ne
  where
    lastMatch = snd tailMatch
    tailMatch = B.breakSubstring s . B.drop (B.length ls - 20) $ ls
    ve1 = fst . fromMaybe (0, "") $ BC.readInt (B.singleton . B.index ls $ e1)
    ve2 = fst . fromMaybe (0, "") $ BC.readInt (B.singleton . B.index ls $ e2)
    ie1 = 1 + ve1 + e1
    ie2 = 1 + ve2 + e2
    nnums = BC.pack . show $ (ve1 + ve2)
    new = B.length ls + B.length nnums
    ne = ord' ((ie1 `mod` new), (ie2 `mod` new))
    nls = B.concat [ls, nnums]

ord' (a, b)
  | a < b = (a, b)
  | otherwise = (b, a)
