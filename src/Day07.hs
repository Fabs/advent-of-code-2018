{-# OPTIONS_GHC -Wall #-}

module Day07 where

-- consider topological sorting when redoing
import Data.Char (ord)
import Data.List ((\\), concat, minimumBy, nub, sort)
import Data.Ord (comparing)
import Debug.Trace
import Fabs.LazyStdin (runOnInput)
import Text.Regex.Posix

depsRegex :: String -> [String]
depsRegex s = fmap id $ tail . head . matches $ s
  where
    matches :: String -> [[String]]
    matches s' =
      (s' :: String) =~
      "Step (\\w+) must be finished before step (\\w+) can begin." :: [[String]]

data Dep = Dep
  { item :: String
  , dep :: String
  } deriving (Show, Eq, Ord)

parseDeps :: [String] -> [Dep]
parseDeps = fmap $ buildDep . depsRegex

buildDep :: [String] -> Dep
buildDep (a:b:[]) = Dep a b
buildDep _ = error "this is maldormed input"

star1 :: IO ()
star1 = print =<< runOnInput (buildOrder . parseDeps)
  where
    buildOrder originalPlan = plan originalPlan [] originalPlan

plan :: [Dep] -> [String] -> [Dep] -> String
plan orig s [] = concat s ++ remaining
  where
    remaining = nub . sort $ (nub . concat $ fmap dep orig) \\ concat s
plan orig s deps =
  plan orig (s ++ [buildableInOrder]) (solve deps buildableInOrder)
  where
    buildableInOrder = minimum buildable'
    buildable' = toBuild deps \\ depends deps
    solve depPlan build = filter (\(Dep i _) -> i /= build) depPlan

star2 :: IO ()
star2 = print =<< runOnInput (buildOrder . parseDeps)
  where
    buildOrder orig = planTime 5 (fullPlan orig) (fullPlan orig) [] ([], 0, [])
      where
        fullPlan o =
          o ++ (fmap (\c -> Dep c ".") $ nub (toBuild o ++ depends o))

data Work = Work
  { job :: String
  , time :: Int
  } deriving (Show, Eq, Ord)

planTime ::
     Int
  -> [Dep]
  -> [Dep]
  -> [Work]
  -> ([String], Int, [Work])
  -> (String, Int, [Work])
planTime qmax orig left wk (built, t, steps)
  | fullyBuild = trace "result" (concat built, t, steps)
  | fullQueue = traceShow (length left) freeMin
  | cantProceed = traceShow (length left) freeMin
  | otherwise = traceShow (newWork) planWork
  where
    fullyBuild = null left && null wk
    fullQueue = qmax == inProgress
    cantProceed = 0 == length newWork
    freeMin =
      planTime
        qmax
        orig
        (prog buildMin)
        finishMin
        (buildMin, t + effortMin, steps)
    minWork = minimumBy (comparing time) wk
    finishMin =
      fmap (\(Work j t') -> Work j (t' - time minWork)) . (filter (/= minWork)) $
      wk
    buildMin = built ++ [job minWork]
    effortMin = time minWork
    planWork =
      planTime
        qmax
        orig
        (prog built)
        (newWork ++ wk)
        (built, t, newWork ++ steps)
    newWork = withEffort . sort . take (qmax - inProgress) $ buildable left wk
    prog bt = filter (\(Dep i _) -> notElem i bt) left
    inProgress = length wk
    withEffort = fmap (\s -> Work s (effort s))

effort :: String -> Int
effort "." = 0
effort s = (ord . head $ s) + 60 - 64

buildable :: [Dep] -> [Work] -> [String]
buildable left wk = nub $ (toBuild left \\ depends left) \\ building wk

building :: [Work] -> [String]
building = nub . fmap job

toBuild :: [Dep] -> [String]
toBuild = nub . fmap item

depends :: [Dep] -> [String]
depends = nub . fmap dep
