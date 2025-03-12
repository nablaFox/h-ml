module ML.Model where

type TrainingSet a b = [(a, b)]

type Trainer m a b = m -> TrainingSet a b -> m

class Model m a b where
  train :: m -> TrainingSet a b -> Trainer m a b -> m
  train m set f = f m set
  loss :: m -> TrainingSet a b -> Double
  predict :: m -> a -> b

mse :: (Num a, Num b) => TrainingSet a b -> b
mse = undefined
