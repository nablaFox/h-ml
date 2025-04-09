import Model (showPredictions)
import PolyRegr (trainWithNormalEq)
import Utils.Math (gaussian)

main :: IO ()
main = do
  trainingSet <-
    mapM
      ( \x -> do
          noise <- gaussian 0 0.1
          return (x, sin (2 * pi * x) + noise)
      )
      [0, 0.1 .. 1]

  let (model, accuracy) = trainWithNormalEq 3 trainingSet

  putStrLn ("Finished training. Model accuracy: " ++ show accuracy)

  writeFile "predictions.txt" (showPredictions model [0, 0.01 .. 1.2])

  putStrLn "Predictions saved to predictions.txt"
