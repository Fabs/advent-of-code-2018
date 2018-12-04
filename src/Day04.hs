{-# LANGUAGE FlexibleContexts #-}

module Day04 where

import Data.List as L
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust)
import Fabs.LazyStdin (runOnInput)
import Text.Regex.Posix

data Guard = Guard
  { id :: Int
  , minutes :: Map.Map Int Int
  } deriving (Show)

data Event
  = Shift
  | Asleep
  | Awake
  deriving (Show)

data Id =
  Id Int
  deriving (Show)

data TimeEntry =
  TimeEntry Event
            Int
            Id
  deriving (Show)

parseInt = read :: String -> Int

timeRegex :: String -> (Maybe [String], Event)
timeRegex = head . simplify . pickMatch . matchAny
  where
    simplify :: [(Maybe [[String]], Event)] -> [(Maybe [String], Event)]
    simplify = fmap maybeHead
    pickMatch :: [(Maybe [[String]], Event)] -> [(Maybe [[String]], Event)]
    pickMatch = L.filter (isJust . fst)
    maybeHead :: (Maybe [[String]], Event) -> (Maybe [String], Event)
    maybeHead (Nothing, e) = (Nothing, e)
    maybeHead (Just l, e) = (Just . tail . head $ l, e)
    matchAny :: String -> [(Maybe [[String]], Event)]
    matchAny s = [matchShift, matchAwake, matchAsleep] <*> [s]
    matchShift :: String -> (Maybe [[String]], Event)
    matchShift s =
      ( (s :: String) =~~
        "\\[[0-9]+-[0-9]+-[0-9]+ [0-9]+:([0-9]+)\\] Guard #([0-9]+) begins shift" :: Maybe [[String]]
      , Shift)
    matchAwake :: String -> (Maybe [[String]], Event)
    matchAwake s =
      ( (s :: String) =~~ "\\[[0-9]+-[0-9]+-[0-9]+ [0-9]+:([0-9]+)\\] wakes up" :: Maybe [[String]]
      , Awake)
    matchAsleep :: String -> (Maybe [[String]], Event)
    matchAsleep s =
      ( (s :: String) =~~
        "\\[[0-9]+-[0-9]+-[0-9]+ [0-9]+:([0-9]+)\\] falls asleep" :: Maybe [[String]]
      , Asleep)

parseTime :: [String] -> [TimeEntry]
parseTime = fmap $ parseTimeEntry . timeRegex

-- Ahhh horrible design
parseTimeEntry :: (Maybe [String], Event) -> TimeEntry
parseTimeEntry (Nothing, e) = error $ "parse error" ++ show e
parseTimeEntry (Just (m:id:[]), Shift) =
  TimeEntry Shift (parseInt m) (Id . parseInt $ id)
parseTimeEntry (Just (m:[]), e) = TimeEntry e (parseInt m) (Id 0)

star1 :: IO ()
star1 = print =<< runOnInput solve
  where
    solve = mult . mostSleepy . guards . annotateTime . parseTime . sort
    mostSleepy l = foldr most (0, 0, (0, 0)) l
      where
        most (Guard gid set) max@(id, sleep, _)
          | countSet set > sleep = (gid, countSet set, maxSet set)
          | otherwise = max
        maxSet s = maximum $ map (\(k, v) -> (v, k)) $ Map.toList s
        countSet :: Map.Map Int Int -> Int
        countSet s =
          foldr (\v a -> a + v) (0 :: Int) . (map snd) . Map.toList $ s

star2 :: IO ()
star2 = print =<< runOnInput solve
  where
    solve = mult . mostSleepy . guards . annotateTime . parseTime . sort
    mostSleepy l = foldr most (0, (0 :: Int), (0, 0)) l
      where
        most :: Guard -> (Int, Int, (Int, Int)) -> (Int, Int, (Int, Int))
        most (Guard gid set) mx@(id, r, lastMax)
          | compare set lastMax = (gid, (0 :: Int), maxSet set)
          | otherwise = (id, (0 :: Int), lastMax)
        maxSet s
          | Map.size s == 0 = (0, 0)
          | otherwise =
            maximum $ map (\(k, v) -> (v, k) :: (Int, Int)) $ Map.toList s
        compare :: Map.Map Int Int -> (Int, Int) -> Bool
        compare s max = isBigger smax max
          where
            smax = maxSet s
            isBigger (v, k) (v', k') = v > v'

mult (id, sleep, (times, minute)) = (id, sleep, times, minute, id * minute)

mostSleepy l = foldr most (0, 0, (0, 0)) l
  where
    most (Guard gid set) max@(id, sleep, _)
      | countSet set > sleep = (gid, countSet set, maxSet set)
      | otherwise = max
    maxSet s = maximum $ map (\(k, v) -> (v, k)) $ Map.toList s
    countSet :: Map.Map Int Int -> Int
    countSet s = foldr (\v a -> a + v) (0 :: Int) . (map snd) . Map.toList $ s

guards =
  fst .
  foldl'
    (flip guardMap)
    (Map.empty :: Map.Map Int Guard, Map.empty :: Map.Map Int Int)

guardMap (TimeEntry _ _ (Id 0)) (gs, t) = (gs, t)
guardMap (TimeEntry Asleep m (Id id)) (gs, t) = (gs, Map.insert id m t)
guardMap (TimeEntry Awake m (Id id)) (gs, t) = (mapUpdate id gs, t)
  where
    mapUpdate k gs =
      case Map.lookup k gs of
        Nothing -> error $ "cannot happen" ++ (show k)
        Just (Guard id schedule) -> Map.insert id (Guard id updateSchedule) gs
          where updateSchedule =
                  foldr
                    (\m s -> Map.alter inc m s)
                    schedule
                    [(sleepTime) .. (m - 1)]
                inc Nothing = Just 1
                inc (Just x) = Just $ x + 1
                sleepTime :: Int
                sleepTime =
                  case Map.lookup id t of
                    Nothing -> error "ups"
                    Just x -> x
guardMap (TimeEntry Shift m (Id id)) (gs, t) =
  case Map.lookup id gs of
    Nothing -> (Map.insert id (Guard id Map.empty) gs, t)
    _ -> (gs, t)

annotateTime :: [TimeEntry] -> [TimeEntry]
annotateTime = reverse . fst . foldl' (flip fixId) ([], 0)

fixId (TimeEntry e m (Id 0)) (gs, lid) = ((TimeEntry e m (Id lid)) : gs, lid)
fixId (TimeEntry e m (Id id)) (gs, _) = ((TimeEntry e m (Id id)) : gs, id)
