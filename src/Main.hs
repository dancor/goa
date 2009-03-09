-- wrapper to gnugo

-- unicode-text front-end (done)
-- sdl-graphics front-end (started)
-- happs?-net front-end (no)

import TurnGame hiding (doMove)
import Control.Arrow
import Control.Monad
import Control.Monad.Error
import Control.Monad.Random
import Data.Array.Base
import Data.List
import FUtil
import Gfx
import Go
import System.Console.GetOpt
import System.Environment
import System.Process
import Txt
import qualified PomTree as PMT

data DispMode = DispModeTxt | DispModeGfx

{-
data GoGame
instance Game GoGame GoMv Color DispMode where
-}

data Options = Options {
  optBoardSize :: Int,
  optPlayAs :: Char,
  optDispMode :: DispMode,
  optKomi :: Maybe Float,
  optHandi :: Int
  }

defOpts = Options {
  optBoardSize = 19,
  optPlayAs = 'r',
  optDispMode = DispModeGfx,
  optKomi = Nothing,
  optHandi = 0
  }

options :: [OptDescr (Options -> Options)]
options = [
  Option ['n'] ["boardsize"] (ReqArg (\ a o -> o {optBoardSize = read a}) "n")
    "board size",
  Option ['c'] ["color"] (ReqArg (\ a o -> o {optPlayAs = case a of
    "b" -> 'b'
    "w" -> 'w'
    "a" -> 'a'
    "n" -> 'n'
    "r" -> 'r'
    _ -> error "invalid play as arg (b|w|a|n)"
    }) "b|w|a|n|r")
    "color to play as",
  Option ['t'] ["text"] (NoArg (\ o -> o {optDispMode = DispModeTxt}))
    "unicode text mode",
  Option ['k'] ["komi"] (ReqArg (\ a o -> o {optKomi = Just $ read a}) "n")
    "komi",
  Option ['h'] ["handicap"] (ReqArg (\ a o -> o {optHandi = read a}) "n")
    "handicap"
  ]

main :: IO ()
main = do
  args <- getArgs
  let
    header = "Usage:"
    (opts, moreArgs) = case getOpt Permute options args of
      (o, n, []) -> (foldl (flip id) defOpts o, n)
      (_, _, errs) -> error $ concat errs ++ usageInfo header options
    [] = moreArgs
    bdSize = optBoardSize opts
    komi = case optKomi opts of
      Just n -> n
      Nothing -> if bdSize >= 19 then 7.5 else case bdSize of
        3 -> 9
        4 -> 2
        5 -> 25
        6 -> 4
        _ -> 5.5
    handi = optHandi opts
    cmd = "gnugo --mode gtp --boardsize " ++ show (optBoardSize opts) ++
      " --komi " ++ show komi
  playAs <- case optPlayAs opts of
    'r' -> evalRandIO $ choice "bw"
    playAs' -> return playAs'
  let
    pl = case playAs of
      'b' -> [Human, Comm 0]
      'w' -> [Comm 0, Human]
      'a' -> [Human, Human]
      'n' -> [Comm 0, Comm 0]
    (initF, dispF) = case optDispMode opts of
      DispModeTxt -> (($ error "incorrect gfx access"), gameDisp TxtDisp)
      DispModeGfx -> (Gfx.withGfx $ optBoardSize opts, gameDisp GfxDisp)
    mid = join (,) $ bdSize `div` 2 + 1
    handiMoves i = if i /= 0 && i < 2 || i > 9
      then error "Handicap must be >= 2 and <= 9." else case i of
        5 -> handiMoves 4 ++ [mid]
        7 -> handiMoves 6 ++ [mid]
        _ -> take i $ [(p3, p3), (p1, p1), (p1, p3), (p3, p1), (p1, p2),
          (p3, p2), (p2, p3), (p2, p1), (p2, p2)]
      where
        p1 = 4
        p2 = bdSize `div` 2 + 1
        p3 = bdSize + 1 - 4
  initF $ \ gH -> do
    proc@(inp, out, err, pid) <- runInteractiveCommand cmd
    let
      gos = GoState (dispF gH) (optBoardSize opts) pl [proc] PMT.empty
    histOrErr <- runErrorT $ doTurn =<< (foldM (flip doMove) gos .
      intersperse Pass . map Play $ handiMoves handi)
    case histOrErr of
      Left err -> putStrLn err
      Right hist -> putStrLn "bye"
