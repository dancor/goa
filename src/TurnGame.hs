{-# LANGUAGE MultiParamTypeClasses #-}

module TurnGame where

import Control.Monad.Error
import qualified PomTree as PMT

-- a turn-based game such as go or chinese checkers, plus:
-- - branching history
-- - computer/network player support
class TurnGame gm mv where
  -- do a move and tell computer/network players.
  doMove :: mv -> gm -> ErrorT String IO gm
  -- generate a move for the current player.
  genMove :: gm -> ErrorT String IO mv
  -- generate a computer move even if the current player is a human.
  -- intended to be used by genMove or to get a "hint" from the engine.
  compGenMove :: gm -> ErrorT String IO mv
  whoseTurn :: gm -> player

  --readMove  :: String -> gm -> Either String mv
  -- get a move from a computer/network player
  --compMove  :: gm -> ErrorT String IO (gm, mv)
  {-
  --initDisp  :: gm -> IO dispH
  --disp      :: dispH -> gm -> PMT.PomTree mv  -> IO ()

  playGame  :: (dispH -> gm -> PMT.PomTree mv -> IO ()) -> gm ->
    ErrorT String IO (PMT.OmTree mv, PMT.OmTreeContext mv)
  --playGame disp gm =
  -}

  playGame :: gm -> ErrorT String IO (PMT.OmTree mv, PMT.OmTreeContext mv)
  playGame gm = do
    getMove
    compMove

  -- this can be replaced with a more direct version (in the Go Text Protocol,
  -- e.g., compGenMove needs to be 'genmove+undo' whereas compMove can just be
  -- 'genmove').
  -- intended to be used by doMove
  compMove :: gm -> ErrorT String IO gm
  compMove gm = flip doMove gm =<< compGenMove gm
