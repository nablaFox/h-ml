module Utils.Data where

import Utils.Math

data KDTree a b
  = Empty
  | Leaf ([a], b)
  | Node {axis :: Int, value :: a, left :: KDTree a b, right :: KDTree a b}
  deriving (Show)

createKDTree :: (Ord a) => Int -> [([a], b)] -> KDTree a b
createKDTree _ [] = Empty
createKDTree _ [x] = Leaf x
createKDTree depth xs = Node ax m (createKDTree newDepth left) (createKDTree newDepth right)
  where
    ax = depth `mod` length (fst (head xs))
    m = median (map (\x -> fst x !! ax) xs)
    left = filter (\(x, _) -> x !! ax < m) xs
    right = filter (\(x, _) -> x !! ax >= m) xs
    newDepth = depth + 1
