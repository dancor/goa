-- wrapper to gnugo

-- unicode-text front-end (done)
-- sdl-graphics front-end (started)
-- happs?-net front-end (no)

import Control.Arrow
import Control.Monad
import Data.Array.Base
import Data.IntMap
import Data.List
import Gfx
import Go
import System.Console.GetOpt
import System.Environment
import System.Process
import Txt
import qualified PosMTree as PMT

data Flag = BoardSize String | PlayAs String | TextDisp deriving Show
type OptVals = (Int, Char, Bool)

options :: [OptDescr Flag]
options = [
  Option ['n'] ["boardsize"] (ReqArg BoardSize "n") "board size",
  Option ['c'] ["color"] (ReqArg PlayAs "b|w|a|n") "color to play as",
  Option ['t'] ["text"] (NoArg TextDisp) "unicode text mode"
  ]

procOpt :: Flag -> OptVals -> OptVals
procOpt f (bdN, c, gfx) = case f of
  BoardSize bdN' -> (read bdN', c, gfx)
  PlayAs c' -> case c' of
    "b" -> (bdN, 'b', gfx)
    "w" -> (bdN, 'w', gfx)
    "a" -> (bdN, 'a', gfx)
    "n" -> (bdN, 'n', gfx)
    _ -> error "invalid play as arg (b|w|a|n)"
  TextDisp -> (bdN, c, False)

main :: IO ()
main = do
  args <- getArgs
  let
    header = "Usage:"
    (opts, moreArgs) = case getOpt Permute options args of
      (o, n, []) -> (o, n)
      (_, _, errs) -> error $ concat errs ++ usageInfo header options
    [] = moreArgs
    (bdN, c, gfx) = foldr procOpt (19, 'b', True) opts
    comm = "gnugo --mode gtp --boardsize " ++ show bdN
    pl = case c of
      'b' -> [Human, Comm 0]
      'w' -> [Comm 0, Human]
      'a' -> [Human, Human]
      'n' -> [Comm 0, Comm 0]
    dispF = if gfx then gameDisp GfxDisp else gameDisp TxtDisp
    initF = if gfx then Gfx.withGfx bdN else ($ error "incorrect gfx access")
  initF $ \ gH -> do
    (inp, out, err, pid) <- runInteractiveCommand comm
    -- kill the gfx..
    histOrErr <- doTurn (dispF gH) bdN pl [(inp, out, err, pid)] PMT.empty
    case histOrErr of
      Right hist -> do
        putStrLn "bye"
      Left err -> do
        putStrLn err
