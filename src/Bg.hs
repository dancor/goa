module Bg where

import Data.Array

data BgType = BgReg Ordering Ordering | BgStar deriving Show

nToBg :: Int -> Array (Int, Int) BgType
nToBg n
 | n < 2 || n > 19 = error "Board size must currently be >= 2 and <= 19."
 | n >= 12 && n `div` 2 == 0 = fourStarAt 4 `starify` noStar
 | n >= 8 && n `div` 2 == 0 = fourStarAt 3 `starify` noStar
 | n >= 13 = nineStarAt 4 `starify` noStar
 | n >= 7 = fiveStarAt 3 `starify` noStar
 | otherwise = noStar where
  fourStarAt i = allPair [i, n + 1 - i]
  fiveStarAt i = fourStarAt i ++ allPair [n `div` 2 + 1]
  nineStarAt i = allPair [i, n `div` 2 + 1, n + 1 - i]
  allPair xs = [(x, y) | x <- xs, y <- xs]
  noStar = listArray ((1, 1), (n, n)) . concat . map reverse $
    [row LT] ++ replicate (n - 2) (row EQ) ++ [row GT]
  row vert =
    [BgReg LT vert] ++ replicate (n - 2) (BgReg EQ vert) ++ [BgReg GT vert]
  starify stars bg = bg // (map (flip (,) BgStar) stars)
