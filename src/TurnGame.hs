{-# LANGUAGE MultiParamTypeClasses #-}

module TurnGame where

import Control.Monad.Error
import qualified Data.PomTree as PMT

type CanErrStr a = Either String a
type CanErrStrIO a = ErrorT String IO a

-- a turn-based game such as go or chinese checkers, plus:
-- - game control commands (like undo and quit)
-- - branching history
-- - computer/network player support with async commands
--   (i.e. wait for computer moves in a separate thread)
-- - move input
--class TurnGame gm mv command where
class TurnGame gm mv where
  -- do a move and tell computer/network players.
  doMove :: mv -> gm -> ErrorT String IO gm
  {-
  -- generate a move for the current player.
  genMove :: gm -> ErrorT String IO mv
  -- generate a computer move even if the current player is a human.
  -- intended to be used by genMove or to get a "hint" from the engine.
  genCompMove :: gm -> ErrorT String IO mv
  getInp :: gm -> ErrorT String IO (Either mv command)
  -- return Nothing if the user quits
  handleCommand :: command -> gm -> ErrorT String IO (Maybe gm)
  inpMove :: gm -> ErrorT String IO mv
  whoseTurn :: gm -> player
  doUndo :: gm -> ErrorT String IO gm
  -}

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
  {-
  playGame gm = do
    getMove
    compMove

  -- this can be replaced with a more direct version (in the Go Text Protocol,
  -- e.g., genCompMove needs to be 'genmove+undo' whereas compMove can just be
  -- 'genmove').
  -- intended to be used by doMove
  compMove :: gm -> ErrorT String IO gm
  compMove gm = flip doMove gm =<< compGenMove gm
  -}

{-
class (TurnGame gm mv) => CommTurnGame gm mv where


class TurnGame gm mv where

  -- convention: zero-indexed
  -- todo?: allow for game-completed states here as well?
  whoseTurn :: gm -> Either Int

  readMove :: String -> gm -> CanErrStr mv

  doMove :: mv -> gm -> gm
-}
