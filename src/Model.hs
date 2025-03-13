module Model where

type Hypothesis a b = a -> b

data Model a b p = Model {predict :: Hypothesis a b, params :: p}

type Trainer z a b p = z -> (Model a b p, Double)

-- supervised learning: 

type SupervisedTrainer a b p = Trainer [(a, b)] a b p

type Loss a b p = Model a b p -> [(a, b)] -> Double

mse :: (Num a) => Loss a Double p
mse model env = sum $ map (\(x, y) -> (y - predict model x) ^ 2) env

crossEntropy :: Loss a [Double] p
crossEntropy = undefined

showPredictions :: (Show a, Show b) => Model a b p -> [a] -> String
showPredictions model test = unlines $ map (\x -> show x ++ "  " ++ show (predict model x)) test
