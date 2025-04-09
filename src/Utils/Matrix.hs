module Utils.Matrix where

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

showDim :: Matrix a -> String
showDim m = show (nrows m) ++ "x" ++ show (ncols m)

matrix :: [[a]] -> Matrix a
matrix xs = Matrix (length xs) (length $ head xs) xs

matrix' :: [[a]] -> Matrix a
matrix' = transpose . matrix

cols :: Matrix a -> [[a]]
cols = transposeList . rows

transposeList :: [[a]] -> [[a]]
transposeList xs
  | any null xs = []
  | otherwise = map head xs : transposeList (map tail xs)

transpose :: Matrix a -> Matrix a
transpose = matrix . transposeList . rows

dotList :: (Num a) => [a] -> [a] -> a
dotList v1 v2
  | length v1 /= length v2 = error "Dot product requires lists of equal length"
  | otherwise = sum $ zipWith (*) v1 v2

-- (1 x n) `dot` (n x 1)
dot :: (Num a) => Matrix a -> Matrix a -> a
dot m1 m2 = rows (m1 * m2) !! 0 !! 0

multiply :: (Num a) => Matrix a -> Matrix a -> Matrix a
multiply m1 m2 = matrix [[r `dotList` c | c <- cols m2] | r <- rows m1]

instance (Num a) => Num (Matrix a) where
  (+) = undefined -- TEMP
  (*) = multiply
  (-) = undefined -- TEMP

  abs = undefined
  signum = undefined
  fromInteger = undefined

-- TEMP: Currently implemented by GPT-o3; I'll get to this soon

invert :: (Fractional a, Eq a) => Matrix a -> Matrix a
invert m
  | nrows m /= ncols m = error "invert: Matrix must be square"
  | otherwise = Matrix n n invRows
  where
    n = nrows m
    identity = [[if i == j then 1 else 0 | j <- [0 .. n - 1]] | i <- [0 .. n - 1]]
    aug = zipWith (++) (rows m) identity
    reduced = gaussJordan aug 0 n
    invRows = map (drop n) reduced

gaussJordan :: (Fractional a, Eq a) => [[a]] -> Int -> Int -> [[a]]
gaussJordan mat col n
  | col >= n = mat
  | otherwise =
      case findPivot mat col col of
        Nothing -> error "invert: Matrix is singular and cannot be inverted"
        Just pivotRowIndex ->
          let matSwapped =
                if pivotRowIndex /= col
                  then swapRows mat col pivotRowIndex
                  else mat
              pivotRow = matSwapped !! col
              pivotElem = pivotRow !! col
              newPivotRow = map (/ pivotElem) pivotRow
              matNormalized = replaceRow matSwapped col newPivotRow
              matEliminated =
                foldl
                  ( \m' r ->
                      if r == col
                        then m'
                        else
                          let factor = (m' !! r) !! col
                              newRow = zipWith (\x y -> x - factor * y) (m' !! r) newPivotRow
                           in replaceRow m' r newRow
                  )
                  matNormalized
                  [0 .. n - 1]
           in gaussJordan matEliminated (col + 1) n

findPivot :: (Eq a, Num a) => [[a]] -> Int -> Int -> Maybe Int
findPivot mat col start = go start
  where
    go i
      | i >= length mat = Nothing
      | (mat !! i) !! col /= 0 = Just i
      | otherwise = go (i + 1)

swapRows :: [[a]] -> Int -> Int -> [[a]]
swapRows mat i j = map swap (zip [0 ..] mat)
  where
    swap (k, row)
      | k == i = mat !! j
      | k == j = mat !! i
      | otherwise = row

replaceRow :: [[a]] -> Int -> [a] -> [[a]]
replaceRow mat i newRow = take i mat ++ [newRow] ++ drop (i + 1) mat
