module ML.PolyRegr where

import ML.Model
import Utils.Matrix

designMatrix :: (Num a) => Int -> [a] -> Matrix a
designMatrix deg xs = matrix [[x^i | i <- [0..deg]] | x <- xs]

trainWithNormalEq :: (Fractional a, Eq a, Real a) => SupervisedTrainer Int (Matrix a) a a
trainWithNormalEq deg trset 
  | deg > length xs = error "The number of feature points must be greater or equal to the degree"
  | otherwise = trainedModel model weights trset 
  where
    (xs, ts) = unzip trset 
    designMat = designMatrix deg xs
    designMatT = transpose designMat
    weights = invert (designMatT * designMat) * designMatT * matrix' [ts]
    model = newModel (\x -> designMatrix deg [x] `dot` weights) se
