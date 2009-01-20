module Txt where

import Control.Arrow
import Data.Array
import Data.List
import DispUtil
import FUtil
import Go
import qualified AnsiColor as AC
import qualified PomTree as PMT

data TxtDisp = TxtDisp
instance GameDisp TxtDisp where
  gameDisp _ gH bdN hist = putStrLn $ showHist bdN hist

-- show board state with highlighted last move
-- currently only nxn with n in [9, 13, 19] supported for print out
showBdHS :: (BdH, Array Color Int) -> String
showBdHS (bd, capd) = toUtf $ bg ++
  cap header (unlines $ zipWith cap (map
    (padl ' ' logBdN . show)
    (reverse [1..bdN])) (map (cap " " . intercalate "─") filledBd)) ++
  unlines [
    "Blk has capd " ++ show (capd ! Wht),
    "Wht has capd " ++ show (capd ! Blk)] ++
  AC.normal where
  ((xMin, yMin), (xMax, yMax)) = bounds bd
  bdN = yMax - yMin + 1
  r = replicate
  m = case bdN of
    19 -> r 2 e ++ f ++ r 5 e ++ f ++ r 5 e ++ f ++ r 2 e
    13 -> r 2 e ++ f ++ r 2 e ++ f ++ r 2 e ++ f ++ r 2 e
    9 -> r 1 e ++ f ++ r 3 e ++ f ++ r 1 e
    7 -> r 1 e ++ f ++ r 1 e ++ f ++ r 1 e
  e = ["├"] ++ r (bdN - 2) "┼" ++ ["┤"]
  f = case bdN of
    19 -> [["├"] ++ r 2 "┼" ++ d ++ r 5 "┼" ++ d ++ r 5 "┼" ++
      d ++ r 2 "┼" ++ ["┤"]]
    13 -> [["├"] ++ r 2 "┼" ++ d ++ r 2 "┼" ++ d ++ r 2 "┼" ++
      d ++ r 2 "┼" ++ ["┤"]]
    9 -> [["├"] ++ r 1 "┼" ++ d ++ r 3 "┼" ++
      d ++ r 1 "┼" ++ ["┤"]]
    7 -> [["├"] ++ r 1 "┼" ++ d ++ r 1 "┼" ++
      d ++ r 1 "┼" ++ ["┤"]]
  empBd = [["┌"] ++ r (bdN - 2) "┬" ++ ["┐"]] ++ m ++
    [["└"] ++ r (bdN - 2) "┴" ++ ["┘"]]
  filledBd = zipWith (zipWith (\fill emp -> case fill of
      (Emp, _) -> emp
      (Stone Blk, True) -> bl
      (Stone Blk, False) -> b
      (Stone Wht, True) -> wl
      (Stone Wht, False) -> w
    )) (reverse $ stripe bdN $ elems bd) empBd
  b = AC.blue ++ "O" ++ AC.reset ++ bg
  w = AC.yellow ++ "@" ++ AC.reset ++ bg
  bl = AC.blue ++ AC.bold ++ "O" ++ AC.reset ++ bg
  wl = AC.yellow ++ AC.bold ++ "@" ++ AC.reset ++ bg
  d = ["•"]
  bg = AC.red ++ AC.blackBg
  logBdN = if bdN >= 10 then 2 else 1
  header = " " ++ replicate logBdN ' ' ++ intercalate " "
    --(map (:[]) (take bdN $ ['a'..'h'] ++ ['j'..'z'])) ++ "\n"
    (map (:[]) (take bdN $ ['A'..'H'] ++ ['J'..'Z'])) ++ "\n"

showHist :: Int -> Hist -> String
showHist bdN h = showBdHS $ first (bdHilight l) (bdDoMoves bdStInit p)
  where
  p = PMT.getPath h
  l = if null p then Pass else last p
  bdStInit = (listArray ((1, 1), (bdN, bdN)) $ repeat Emp,
    listArray (Blk, Wht) $ repeat 0)
