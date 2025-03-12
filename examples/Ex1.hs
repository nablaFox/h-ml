module Main (main) where

import ML.PolyRegr (trainWithNormalEq)
import Utils.Math (gaussian)
import ML.Model (makePredictions)

formatPrediction :: (Show a, Show b, Floating a) => (a, b) -> String
formatPrediction (x, y) = show x ++ " " ++ show y ++ " " ++ show (sin (2*pi*x))

main :: IO ()
main = do
  trainingSet <- mapM (\x -> do
    noise <- gaussian 0 0.1
    return (x, sin (2*pi*x) + noise)
    ) [0,0.1..1]

  let deg = 3

  let (model, _, err) = trainWithNormalEq deg trainingSet 

  let predictions = makePredictions model [0,0.01..1.1]

  putStrLn ("Finished training with error: " ++ show err)

  writeFile "predictions.txt" (unlines (map formatPrediction predictions))

  putStrLn "Predictions saved to predictions.txt"
