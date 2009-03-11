{-# LANGUAGE MultiParamTypeClasses #-}

module TurnGame where

import Control.Monad.Error
import qualified PomTree as PMT

-- a turn-based game such as go, chess, or chinese checkers
-- with branching undo and possibly computer/network players
class TurnGame gm mv player dispH where
  readMove  :: String -> gm -> Either String mv
  doMove    :: mv -> gm -> Either String bd
  whoseTurn :: gm -> player
  -- do a move and tell computer/network players
  doMoveIO  :: mv -> gm -> ErrorT String IO gm
  -- get a move from a computer/network player
  compMove  :: gm -> ErrorT String IO (gm, mv)
  initDisp  :: gm -> IO dispH
  disp      :: dispH -> gm -> PMT.PomTree mv  -> IO ()
  playGame  :: (dispH -> gm -> PMT.PomTree mv -> IO ()) -> gm ->
    ErrorT String IO (PMT.OmTree mv, PMT.OmTreeContext mv)
