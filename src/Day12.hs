module Day12 where

import qualified Data.Map.Strict as Map
import Debug.Trace
import Fabs.LazyStdin (runOnInput)

star1 :: IO ()
star1 = print =<< runOnInput solve
  where
    solve (start:instructions) = (simulate 20 start (compile instructions) 0)

compile :: [String] -> Map.Map String Char
compile = foldr splitInsert Map.empty
  where
    splitInsert s = Map.insert ins result
      where
        ins = take 5 s
        result = (!! 6) s

count = length . filter (== '#')

simulate 0 s _ c = c
simulate k s h c = traceShow (pls) simulate (k - 1) pls h (c + count pls)
  where
    grow pv _ "..." acc = acc ++ [(Map.!) h (pv ++ "...")]
    grow pv@(_:p:[]) nx fs@(s':s2':s3':s4':ss') acc =
      grow (p : s' : []) (s3' : s4' : []) (s2' : s3' : s4' : ss') (acc ++ [pl])
      where
        pl = (Map.!) h (pv ++ [s'] ++ nx)
    plant s' = grow b a st ""
      where
        b = take 2 $ st
        a = take 2 . drop 3 $ st
        st = drop 2 $ "....." ++ s' ++ "....."
    pls = plant s
