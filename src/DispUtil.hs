module DispUtil where

import Data.Array
import FUtil
import Go

bdDoMove :: BdState -> (Move, Color) -> BdState
bdDoMove bd@(b, capd) (move, color) = case move of
  Pass -> bd
  Play i@(x, y) -> checkKill i (b // [(i, Stone color)], capd)

bdDoMoves :: BdState -> [Move] -> BdState
bdDoMoves bd moves = foldl bdDoMove bd $ zip moves $ iterate cycSucc Blk

