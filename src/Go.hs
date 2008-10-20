module Go where

import qualified Control.Arrow as CA
import qualified Control.Monad as CM
import qualified FUtil as Dnl
import qualified Data.Array as Arr
import qualified Data.Char as Chr
import Data.List
import qualified Data.Maybe as Mby
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as Img
import qualified Paths_goa as Paths
import qualified PosMTree as PMT
import qualified System.Console.GetOpt as Opt
import System.Environment
import qualified System.IO as SIO
import qualified System.Process as SP

import qualified AnsiColor as C

-- show board state with highlighted last move
-- currently only nxn with n in [9, 13, 19] supported for print out
showBdHS :: (BdH, Arr.Array Color Int) -> String
showBdHS (bd, capd) = Dnl.toUtf $ bg ++
  Dnl.cap header (unlines $ zipWith Dnl.cap (map
    (Dnl.padl ' ' logBdN . show)
    (reverse [1..bdN])) (map (Dnl.cap " " . intercalate "─") filledBd)) ++
  unlines [
    "Blk has capd " ++ show (capd Arr.! Wht),
    "Wht has capd " ++ show (capd Arr.! Blk)] ++
  C.normal where
    ((xMin, yMin), (xMax, yMax)) = Arr.bounds bd
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
      )) (reverse $ Dnl.stripe bdN $ Arr.elems bd) empBd
    b = C.blue ++ "O" ++ C.reset ++ bg
    w = C.yellow ++ "@" ++ C.reset ++ bg
    bl = C.blue ++ C.bold ++ "O" ++ C.reset ++ bg
    wl = C.yellow ++ C.bold ++ "@" ++ C.reset ++ bg
    d = ["•"]
    bg = C.red ++ C.blackBg
    logBdN = if bdN >= 10 then 2 else 1
    header = " " ++ replicate logBdN ' ' ++ intercalate " "
      --(map (:[]) (take bdN $ ['a'..'h'] ++ ['j'..'z'])) ++ "\n"
      (map (:[]) (take bdN $ ['A'..'H'] ++ ['J'..'Z'])) ++ "\n"
imgNames = map (++ ".png") $ ["wood", "black", "white"] ++
  [v ++ h | v <- ["t", "m", "b"], h <- ["l", "m", "r"]]

type BdPos = (Int, Int)
type Comm = (SIO.Handle, SIO.Handle, SIO.Handle, SP.ProcessHandle)
data Player = Human | Comm Int
data Move = Pass | Play BdPos deriving (Show, Ord, Eq)
data Inp = InpMv Move | Undo | Go | GoAll | Quit
data Color = Blk | Wht deriving (Eq, Bounded, Enum, Ord, Arr.Ix, Show)
data BdFill = Emp | Stone Color
type Bd = Arr.Array BdPos BdFill
type BdH = Arr.Array BdPos (BdFill, Bool)
type BdState = (Bd, Arr.Array Color Int)
type Hist = PMT.PosMTree Move

readPosInt :: String -> Maybe Int
readPosInt s = if not (null s) && all Chr.isDigit s then Just $ read s else Nothing

bdHilight :: Move -> Bd -> BdH
bdHilight m b = case m of
  Pass -> fmap (\p -> (p, False)) b
  Play i -> (fmap (\p -> (p, False)) b) Arr.// [(i, (b Arr.! i, True))]

-- should i just use data for hist?
showHist :: Int -> Hist -> Bool -> String
showHist bdN h gfx = let
    p = PMT.getPath h
    l = if null p then Pass else last p
    bdStInit = (Arr.listArray ((1, 1), (bdN, bdN)) $ repeat Emp,
      Arr.listArray (Blk, Wht) $ repeat 0)
  in showBdHS $ CA.first (bdHilight l) (doMoves bdStInit $ p)

parseInp :: String -> Either String Inp
parseInp s
 | s == "p" || s == "pass" = Right $ InpMv $ Pass
 | s == "q" || s == "quit" = Right $ Quit
 | s == "u" || s == "undo" = Right $ Undo
 | s == "g" || s == "go" = Right Go
 | s == "ga" || s == "goall" = Right GoAll
 | s == "" = Left $ "empty string?"
 | otherwise =
  let ([s1], s2) = splitAt 1 s
      xRaw = Chr.ord $ Chr.toLower s1
      x = xRaw - Chr.ord 'a' + if xRaw > Chr.ord 'i' then 0 else 1
      y = readPosInt s2 in
    case y of
      Nothing -> Left "could not parse"
      Just yy -> if yy < 1 || yy > 19 then Left "out of range" else Right $ InpMv $ Play (x, yy)

del i = uncurry (++) . CA.second tail . splitAt i
isComm p = case p of
  Comm _ -> True
  _ -> False

whileM :: Monad m => m Bool -> m b -> m ()
whileM test f = test >>= \t -> CM.when t $ f >> whileM test f

doUndo :: Int -> [Player] -> Hist -> [Comm] -> IO Hist
doUndo bdN pl hist@(_, ctx) otherComms = if ctx == PMT.Top
  then do
    print "already at beginning of game"
    return hist
  else do
    mapM_ (\p@(inp, out, err, pid) -> do
      SIO.hPutStrLn inp "undo"
      SIO.hFlush inp
      s <- SIO.hGetLine out
      putStrLn s
      s <- SIO.hGetLine out
      putStrLn s

      SIO.hPutStrLn inp "showboard"
      SIO.hFlush inp
      {-
      hWaitForInput out 10
      whileM (hReady out) $ hGetLine out >>= putStrLn
      -}
      whileM (SIO.hWaitForInput out 10) $ SIO.hGetLine out >>= putStrLn
      ) otherComms
    return $ Mby.fromJust $ PMT.ascend hist

doTurn :: Int -> [Player] -> [Comm] -> Hist -> Bool -> IO (Either String Hist)
doTurn bdN pl comms hist@(_, ctx) gfx = let
    plN = (length $ PMT.getPath hist) `mod` (length pl)
    quit = return $ Right $ hist
  in if isGameOver hist then quit else do
    putStrLn $ showHist bdN hist gfx
    case pl!!plN of
      Comm n -> let
          p@(inp, out, err, pid) = comms!!n
          -- TODO: not using this?  no multicomm support
          otherComms = del n comms in do
        SIO.hPutStrLn inp "genmove w"
        SIO.hFlush inp
        s <- SIO.hGetLine out
        putStrLn s
        s2 <- SIO.hGetLine out
        putStrLn s2
        if "= " `isPrefixOf` s
          then do
            case parseInp $ map Chr.toLower $ drop 2 s of
              Left err -> do
                return $ Left err
              Right (InpMv move) -> do
                --putStrLn $ show move
                -- TODO: tell other comms here
                doTurn bdN pl comms (PMT.descAdd move hist) gfx
          else do
            return $ Left $ "unexpected response: " ++ s
      Human -> do
        mv <- Dnl.repInp "move: " parseInp
        case mv of
          Undo -> do
            hist' <- doUndo bdN pl hist comms
            hist'' <- doUndo bdN pl hist' comms
            doTurn bdN pl comms hist'' gfx
          Quit -> quit
          -- FIXME?: 2-pl spec, is that ok, assumes exist. of comm 0
          Go -> doTurn bdN ((if plN == 0 then id else reverse) [Comm 0, Human])
            comms hist gfx
          GoAll -> doTurn bdN [Comm 0, Comm 0] comms hist gfx
          InpMv m -> let
              cont = doTurn bdN pl comms (PMT.descAdd m hist) gfx
              errC = doTurn bdN pl comms hist gfx in case m of
            Play (x, y) -> if x < 1 || y < 1 || x > bdN || y > bdN
              then do
                putStrLn "move is not on board"
                errC
              else do
                ss <- mapM (\p@(inp, out, err, pid) -> do
                  SIO.hPutStrLn inp $ "play b " ++
                    [Chr.chr $ Chr.ord 'a' + x - if x < 9 then 1 else 0] ++
                    show y
                  SIO.hFlush inp
                  s1 <- SIO.hGetLine out
                  putStrLn s1
                  s2 <- SIO.hGetLine out
                  putStrLn s2
                  return s1) comms
                if "?" `isPrefixOf` head ss
                  then errC
                  else cont
            Pass -> cont

isGameOver h = let p = PMT.getPath h in
  drop ((length p) - 4) p == replicate 4 Pass

-- captures
-- notice illegal moves (with gnugo?)

isEmp Emp = True
isEmp _ = False

gpLibs :: [BdPos] -> Bd -> [BdPos]
gpLibs gp b = filter (\x -> isEmp $ b Arr.! x) . nub . concat . map (nbrs b) $ gp

nbrs :: Bd -> BdPos -> [BdPos]
nbrs b (x, y) = let
    ((xMin, yMin), (xMax, yMax)) = Arr.bounds b
    bdN = yMax - yMin + 1 in
  (if x > 1 then [(x - 1, y)] else []) ++
  (if y > 1 then [(x, y - 1)] else []) ++
  (if x < bdN then [(x + 1, y)] else []) ++
  (if y < bdN then [(x, y + 1)] else [])

getGp :: BdPos -> Bd -> [BdPos]
getGp i b = case b Arr.! i of
  Emp -> []
  Stone c -> growGp i c b []
growGp :: BdPos -> Color -> Bd -> [BdPos] -> [BdPos]
growGp i c b gp = case b Arr.! i of
  Stone c' -> if c' == c then if null $ filter (== i) gp
      then Dnl.dlof (gp ++ [i]) $ map (\x -> growGp x c b) $ nbrs b i
      else gp
    else gp
  _ -> gp

checkDead :: BdPos -> BdState -> BdState
checkDead i bd@(b, capd) = case b Arr.! i of
  Emp -> bd
  Stone c -> let g = getGp i b in
    if null (gpLibs g b)
       then (b Arr.// [(i, Emp) | i <- g], capd Arr.// [(c, capd Arr.! c + length g)])
       else bd

checkKill :: BdPos -> BdState -> BdState
checkKill i bd@(b, capd) = Dnl.dlof bd $ map checkDead $ nbrs b i

doMove :: BdState -> (Move, Color) -> BdState
doMove bd@(b, capd) (move, color) = case move of
  Pass -> bd
  Play i@(x, y) -> checkKill i (b Arr.// [(i, Stone color)], capd)

doMoves :: BdState -> [Move] -> BdState
doMoves bd moves = foldl doMove bd $ zip moves $ iterate Dnl.cycSucc Blk

data Flag = BoardSize String | PlayAs String | Ascii deriving Show
type OptVals = (Int, Char, Bool)
options :: [Opt.OptDescr Flag]
options = [
  Opt.Option ['n'] ["boardsize"] (Opt.ReqArg BoardSize "n") "board size",
  Opt.Option ['c'] ["color"] (Opt.ReqArg PlayAs "b|w|a|n") "color to play as",
  Opt.Option ['a'] ["ascii"] (Opt.NoArg Ascii) "no gfx"]
procOpt :: Flag -> OptVals -> OptVals
procOpt f (bdN, c, gfx) = case f of
  BoardSize bdN' -> (read bdN', c, gfx)
  PlayAs c' -> case c' of
    "b" -> (bdN, 'b', gfx)
    "w" -> (bdN, 'w', gfx)
    "a" -> (bdN, 'a', gfx)
    "n" -> (bdN, 'n', gfx)
    _ -> error "invalid play as arg (b|w|a|n)"
  Ascii -> (bdN, c, False)
