module KNN where

import Data.List (sortBy)
import Data.Ord
import Model
import Utils.Data
import Utils.Math

type KNNTree a = KDTree a String

findKNearest :: (Floating a, Ord a) => Int -> KNNTree a -> [a] -> [(String, a)]
findKNearest _ Empty _ = [("", 1 / 0)]
findKNearest _ (Leaf (y, label)) x = [(label, l2dist x y)]
findKNearest k (Node ax val left right) x = ks
  where
    curr = x !! ax

    exploreOther = abs (curr - val) < snd (last ks1)

    ks1
      | curr >= val = findKNearest k right x
      | otherwise = findKNearest k left x

    ks2
      | exploreOther && curr >= val = findKNearest k left x
      | exploreOther && curr < val = findKNearest k right x
      | otherwise = []

    ks = take k $ sortBy (comparing snd) (ks1 ++ ks2)

trainEuclideanKNN :: (Ord a, Floating a) => Int -> SupervisedTrainer [a] String (KNNTree a)
trainEuclideanKNN k set
  | k < 0 = error "k must be greater than 0"
  | otherwise = (model, accuracy)
  where
    tree = createKDTree 0 set
    model = Model (fst . mode . findKNearest k tree) tree
    predictions = sum $ map (\x -> fromEnum $ predict model (fst x) == snd x) set
    accuracy = fromIntegral predictions / fromIntegral (length set)
