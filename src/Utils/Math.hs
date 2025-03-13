module Utils.Math where

import System.Random (randomIO)

-- TEMP: GPT generated  (Box–Muller transform)
gaussian :: Double -> Double -> IO Double
gaussian mu sigma = do
  u1 <- randomIO :: IO Double
  u2 <- randomIO :: IO Double
  let z0 = sqrt (-2 * log u1) * cos (2 * pi * u2)
  return (mu + sigma * z0)
