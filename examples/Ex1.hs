module Main (main) where

import ML.KNN (foo)

main :: IO ()
main = do
  print $ foo 3
