module PolyRegr where

import Model
import Utils.Matrix

designMatrix :: (Num a) => Int -> [a] -> Matrix a
designMatrix deg xs = matrix [[x^i | i <- [0..deg]] | x <- xs]

trainWithNormalEq :: Int -> SupervisedTrainer Double Double (Matrix Double)
trainWithNormalEq deg trset 
  | deg > length xs = error "The number of feature points must be greater or equal to the degree"
  | otherwise = (model, mse model trset)
  where
    (xs, ts) = unzip trset 
    designMat = designMatrix deg xs
    designMatT = transpose designMat
    weights = invert (designMatT * designMat) * designMatT * matrix' [ts]
    model = Model (\x -> designMatrix deg [x] `dot` weights) weights
