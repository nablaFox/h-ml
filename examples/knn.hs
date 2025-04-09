import KNN (trainEuclideanKNN)

loadData :: FilePath -> IO [([Double], String)]
loadData = undefined

main :: IO ()
main = do
  trainingSet <- loadData "assets/colors.txt"

  let (model, accuracy) = trainEuclideanKNN 7 []

  putStrLn ("Finished training. Model accuracy: " ++ show accuracy)

-- initialize a loop where we ask the user or to interrupt or to input an RGB value and the program will show the predicted color
