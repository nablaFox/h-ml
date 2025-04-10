import KNN (trainEuclideanKNN)
import Model

readColor :: String -> ([Double], String)
readColor x = (map read rgb :: [Double], unwords ws)
  where
    (rgb, ws) = splitAt 3 (words x)

main :: IO ()
main = do
  let getRGB x = map read (words x) :: [Double]

  colors <- readFile "assets/colors.txt"

  let (model, accuracy) = trainEuclideanKNN 3 (map readColor (lines colors))

  putStrLn ("Finished training. Model accuracy: " ++ show accuracy)

  let userLoop = do
        putStrLn "Enter RGB values separated by spaces (or type 'quit' to exit):"
        input <- getLine
        case input of
          "quit" -> return ()
          _ -> do
            putStrLn $ "Predicted color: " ++ show (predict model (getRGB input))
            userLoop

  userLoop
