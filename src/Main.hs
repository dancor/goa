-- wrapper to gnugo

-- unicode-text front-end (done)
-- sdl-graphics front-end (started)
-- happs?-net front-end (no)

import Control.Arrow
import Control.Monad
import Data.Array.Base
import Data.List
import Gfx
import Go
import System.Console.GetOpt
import System.Environment
import System.Process
import Txt
import qualified PosMTree as PMT

data DispMode = DispModeTxt | DispModeGfx
data Options = Options {
  optBoardSize :: Int,
  optPlayAs :: Char,
  optDispMode :: DispMode,
  optKomi :: Maybe Float
  }

defOpts = Options {
  optBoardSize = 19,
  optPlayAs = 'b',
  optDispMode = DispModeGfx,
  optKomi = Nothing
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
    _ -> error "invalid play as arg (b|w|a|n)"
    }) "b|w|a|n")
    "color to play as",
  Option ['t'] ["text"] (NoArg (\ o -> o {optDispMode = DispModeTxt}))
    "unicode text mode",
  Option ['k'] ["komi"] (ReqArg (\ a o -> o {optKomi = Just $ read a}) "n")
    "komi"
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
    cmd = "gnugo --mode gtp --boardsize " ++ show (optBoardSize opts) ++
      " --komi " ++ show komi
    pl = case optPlayAs opts of
      'b' -> [Human, Comm 0]
      'w' -> [Comm 0, Human]
      'a' -> [Human, Human]
      'n' -> [Comm 0, Comm 0]
    (initF, dispF) = case optDispMode opts of
      DispModeTxt -> (($ error "incorrect gfx access"), gameDisp TxtDisp)
      DispModeGfx -> (Gfx.withGfx $ optBoardSize opts, gameDisp GfxDisp)
  initF $ \ gH -> do
    (inp, out, err, pid) <- runInteractiveCommand cmd
    -- kill the gfx..
    histOrErr <- doTurn (dispF gH) (optBoardSize opts) pl
      [(inp, out, err, pid)] PMT.empty
    case histOrErr of
      Right hist -> do
        putStrLn "bye"
      Left err -> do
        putStrLn err
