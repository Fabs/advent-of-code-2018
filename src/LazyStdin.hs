module LazyStdin
  ( runOnInput
  ) where

runOnInput :: ([String] -> a) -> IO a
runOnInput f = f . lines <$> getContents
