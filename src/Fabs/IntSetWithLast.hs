module Fabs.IntSetWithLast
  ( Set
  , member
  , insert
  , empty
  , safeLast
  , lastIsMember
  ) where

import qualified Data.IntSet as S (IntSet, empty, insert, member, size)
import Data.Maybe (fromMaybe)

data Set = Set
  { set :: S.IntSet
  , lastIns :: Maybe Int
  }

member :: Set -> Int -> Bool
member (Set s (Just l)) s' = l == s' || S.member s' s
member (Set s Nothing) s' = S.member s' s

insert :: Set -> Int -> Set
insert (Set set Nothing) s = Set set (Just s)
insert (Set set (Just s')) s = Set (S.insert s' set) (Just s)

empty :: Set
empty = Set S.empty Nothing

safeLast :: Int -> Set -> Int
safeLast n = fromMaybe n . lastIns

lastIsMember (Set _ Nothing) = False
lastIsMember (Set s (Just s')) = S.member s' s
