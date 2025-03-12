module Utils.Math where

data Matrix a = Matrix {nrows :: Int, ncols :: Int, rows :: [[a]]}

showMatrix :: (Show a) => Matrix a -> String
showMatrix m = unlines $ [topBorder] ++ contentRows ++ [bottomBorder]
  where
    widest = maximum [length (show x) | row <- rows m, x <- row]

    pad s = replicate (widest - length s) ' ' ++ s

    rowStr [] = []
    rowStr (x : xs) = unwords (show x : map (pad . show) xs)

    contentRows = map (\row -> "│ " ++ rowStr row ++ " │") (rows m)

    contentWidth = case rows m of
      (r : _) -> length (rowStr r)
      [] -> 0

    border = replicate contentWidth ' '
    topBorder = "┌ " ++ border ++ " ┐"
    bottomBorder = "└ " ++ border ++ " ┘"

instance (Show a) => Show (Matrix a) where
  show = showMatrix

invert :: (Num a) => Matrix a -> Matrix a
invert m = undefined

transpose :: Matrix a -> Matrix a
transpose = undefined

identity :: Int -> Int -> Matrix Double
identity = undefined

randomMat :: Int -> Int -> Matrix Double
randomMat = undefined

norm :: (Num a) => Matrix a -> Double
norm m = undefined

multiply :: (Num a) => Matrix a -> Matrix a -> Matrix a
multiply = undefined

add :: (Num a) => Matrix a -> Matrix a -> Matrix a
add = undefined

sub :: (Num a) => Matrix a -> Matrix a -> Matrix a
sub = undefined

fromList :: [a] -> Matrix a
fromList = undefined

instance (Num a) => Num (Matrix a) where
  (+) = add
  (*) = multiply
  (-) = sub

  abs = undefined
  signum = undefined
  fromInteger = undefined
