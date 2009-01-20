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
import qualified PomTree as PMT

data DispMode = DispModeTxt | DispModeGfx
data Options = Options {
  optBoardSize :: Int,
  optPlayAs :: Char,
  optDispMode :: DispMode,
  optKomi :: Maybe Float,
  optHandi :: Int
  }

defOpts = Options {
  optBoardSize = 19,
  optPlayAs = 'b',
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
    _ -> error "invalid play as arg (b|w|a|n)"
    }) "b|w|a|n")
    "color to play as",
  Option ['t'] ["text"] (NoArg (\ o -> o {optDispMode = DispModeTxt}))
    "unicode text mode",
  Option ['k'] ["komi"] (ReqArg (\ a o -> o {optKomi = Just $ read a}) "n")
    "komi",
  Option ['h'] ["handicap"] (ReqArg (\ a o -> o {optHandi = read a}) "n")
    "handicap"
  ]

-- fixme make gen if poss or rename etc
{-
foldMM :: (Monad m, Monad n) => (a -> b -> m (n a)) -> a -> [b] -> m (n a)
foldMM _ a []     = return $ return a
--foldMM f a (x:xs) = (f a x >>= \ fax -> foldMM f fax xs)
foldMM f a (x:xs) = do
  a' <- f a x
  foldMM f a' xs
-}
foldMM :: (a -> b -> IO (Either String a)) -> a -> [b] -> IO (Either String a)
foldMM _ a []     = return $ return a
--foldMM f a (x:xs) = (f a x >>= \ fax -> foldMM f fax xs)
foldMM f a (x:xs) = do
  aOrErr <- f a x
  case aOrErr of
    Left err -> return $ Left err
    Right a' -> foldMM f a' xs

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
    pl = case optPlayAs opts of
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
    gosHandiOrErr <- foldMM (flip doMove) gos . intersperse Pass .
      map Play $ handiMoves handi
    case gosHandiOrErr of
      Left err -> putStrLn err
      Right gosHandi -> do
        histOrErr <- doTurn gosHandi
        case histOrErr of
          Right hist -> do
            putStrLn "bye"
          Left err -> do
            putStrLn err
