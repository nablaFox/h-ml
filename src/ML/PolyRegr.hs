module ML.PolyRegr where

import ML.Model
import Utils.Math

newtype PolyRegr = PolynomialRegression {weights :: [Double]}

deg :: PolyRegr -> Int
deg = length . weights

-- TEMP
initPolyRegr :: Int -> PolyRegr
initPolyRegr m = PolynomialRegression (replicate m 0)

designMatrix :: PolyRegr -> [Double] -> Matrix Double
designMatrix model xs
  | m > n = error "Incompatible Model: the number of features must be greater than the degree"
  | otherwise = Matrix n m [[x ^ i | i <- [0 .. m]] | x <- xs]
  where
    n = length xs
    m = deg model

-- solve normal equations
normalTraining :: Trainer PolyRegr Double Double
normalTraining = undefined

instance Model PolyRegr Double Double where
  loss model set = norm (design * w - t) / 2
    where
      (xs, ts) = unzip set
      design = designMatrix model xs
      w = fromList $ weights model
      t = fromList ts

  predict model x = sum [w * (x ^ i) | (i, w) <- zip [0 ..] (weights model)]

-- usage:
-- model = train (initPolyRegr 3) normalTraining
-- predictions = map (predict model) [1,1.1..10]
-- loss = loss model
-- save predictions & loss to file
