-- wrapper to gnugo

import Go
import System.Console.GetOpt
import System.Environment
import System.Process
import qualified PosMTree as PMT

main :: IO ()
main = do
  args <- getArgs
  opts <- case getOpt Permute options args of
    (o, n, []) -> return (o, n)
    (_, _, errs) -> ioError (userError (concat errs ++ usageInfo header options))
      where header = "Usage:"
  --putStrLn $ show opts
  let
    (bdN, c, gfx) = foldr procOpt (19, 'b', True) (fst opts)
    comm = "gnugo --mode gtp --boardsize " ++ show bdN
  (inp, out, err, pid) <- runInteractiveCommand comm
  let
    pl = case c of
      'b' -> [Human, Comm 0]
      'w' -> [Comm 0, Human]
      'a' -> [Human, Human]
      'n' -> [Comm 0, Comm 0]
  histOrErr <- doTurn bdN pl [(inp, out, err, pid)] PMT.empty gfx
  case histOrErr of
    Right hist -> do
      putStrLn "bye"
    Left err -> do
      putStrLn err
