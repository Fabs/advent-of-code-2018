module Fabs.IntSetWithLast
  ( Set
  , member
  , insert
  , empty
  , safeLast
  ) where

import qualified Data.IntSet as S (IntSet, empty, insert, member, size)
import Data.Maybe (fromMaybe)

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

safeLast :: Int -> Set -> Int
safeLast n = fromMaybe n . lastIns
