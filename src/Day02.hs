module Day02 where

import Data.Foldable (fold)
import Data.List (sort)
import Fabs.LazyStdin (runOnInput)

data Check = Check
  { twos :: Int
  , threes :: Int
  } deriving (Show)

instance Semigroup Check where
  (<>) (Check a b) (Check a' b') = Check (a + a') (b + b')

instance Monoid Check where
  mempty = Check 0 0

prodCheck :: Check -> Int
prodCheck p = twos p * threes p

star1 :: IO ()
star1 = print =<< runOnInput checkSum
  where
    checkSum :: [String] -> Int
    checkSum = prodCheck . fold . fmap check
    testSum :: [String] -> [Check]
    testSum = fmap check

star2 :: IO ()
star2 = print =<< runOnInput findMatch
    -- This ALL could be done much better
  where
    findMatch :: [String] -> Maybe String
    findMatch [] = Nothing
    findMatch (s:ss) =
      case diffAllByOne s ss of
        Nothing -> findMatch ss
        Just r -> Just r
    diffAllByOne :: String -> [String] -> Maybe String
    diffAllByOne s' [] = Nothing
    diffAllByOne s' (s:ss) =
      case diffByOne s' s of
        Nothing -> diffAllByOne s' ss
        result -> result
    diffByOne :: String -> String -> Maybe String
    diffByOne s s' = diffR s s' "" 0
      where
        diffR "" "" r _ = Just . reverse $ r
        diffR (c:cs) (c':cs') r l
          | l == 1 && c /= c' = Nothing
          | l == 0 && c /= c' = diffR cs cs' r 1
          | otherwise = diffR cs cs' (c : r) l

check :: String -> Check
check s = checkInit s mempty
  where
    checkInit "" ck = ck
    checkInit (c:cs) ck = checkR cs c 1 "" ck
    checkR :: String -> Char -> Int -> String -> Check -> Check
    checkR "" _ 2 rest (Check 0 b) = checkInit rest $ Check 1 b
    checkR "" _ 3 rest (Check a 0) = checkInit rest $ Check a 1
    checkR "" _ _ rest ck = checkInit rest ck
    checkR (c:cs) ct n r ck
      | c == ct = checkR cs ct (n + 1) r ck
      | otherwise = checkR cs ct n (c : r) ck
