module Utils.Math where

import Data.List
import Data.Ord
import System.Random (randomIO)

-- TEMP: GPT-o3 generated  (Box–Muller transform)
gaussian :: Double -> Double -> IO Double
gaussian mu sigma = do
  u1 <- randomIO :: IO Double
  u2 <- randomIO :: IO Double
  let z0 = sqrt (-2 * log u1) * cos (2 * pi * u2)
  return (mu + sigma * z0)

type Norm a = [a] -> a

lnorm :: (Floating a) => Int -> Norm a
lnorm p xs = sum (map (^ p) xs) ** (1 / fromIntegral p)

l2norm :: (Floating a) => Norm a
l2norm = lnorm 2

type Distance a = [a] -> [a] -> a

ldist :: (Floating a) => Int -> Distance a
ldist p x y = lnorm p (zipWith (-) x y)

l2dist :: (Floating a) => Distance a
l2dist = ldist 2

median :: (Ord a) => [a] -> a
median xs = sort xs !! (length xs `div` 2)

frequency :: (Ord a) => a -> [a] -> Int
frequency el = foldr (\x acc -> fromEnum (x == el) + acc) 0

mode :: (Ord a) => [a] -> a
mode xs = fst $ maximumBy (comparing snd) $ map (\x -> (x, frequency x xs)) xs
