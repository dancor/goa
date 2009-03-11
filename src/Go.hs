{-# LANGUAGE MultiParamTypeClasses #-}

module Go where

import Control.Arrow
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Error
import FUtil
import Data.Array
import Data.Char
import Data.List
import Data.Maybe
import Paths_goa
import System.Console.GetOpt
import System.Environment
import System.IO
import System.Process
import qualified AnsiColor as AC
import qualified PomTree as PMT
import qualified TurnGame as TG

-- (1, 1) is the bottom left
type BdPos = (Int, Int)
type Comm = (Handle, Handle, Handle, ProcessHandle)
data Player = Human | Comm Int
data Move = Pass | Play BdPos
  deriving (Show, Ord, Eq)
data Inp = InpMv Move | Undo | Score | Go | GoAll | Quit
data Color = Blk | Wht
  deriving (Eq, Bounded, Enum, Ord, Ix, Show)
data BdFill = Emp | Stone Color
  deriving (Eq, Show)
type Bd = Array BdPos BdFill
type BdHilight = Array BdPos (BdFill, Bool)
type BdState = (Bd, Array Color Int)
type Hist = PMT.PomTree Move

data GoState = GoState {
  gosDisp :: GoState -> (PMT.OmTree Move, PMT.OmTreeContext Move) -> IO (),
  gosBdN :: Int,
  gosPlayers :: [Player],
  gosComms :: [(Handle, Handle, Handle, ProcessHandle)],
  gosHist :: PMT.PomTree Move
  }

-- data DispH = DispH (TVar (BdHilight, Array Color Int), MVar ())

instance TG.TurnGame GoState Move where
  doMove    = doMove
  playGame  = playGame
  --readMove  = undefined
  --whoseTurn = undefined
  --compMove  = undefined
  --initDisp  = undefined
  --disp      = undefined

readPosInt :: String -> Maybe Int
readPosInt s = if not (null s) && all isDigit s then Just $ read s else Nothing

bdHilight :: Move -> Bd -> BdHilight
bdHilight m b = case m of
  Pass -> fmap (\p -> (p, False)) b
  Play i -> (fmap (\p -> (p, False)) b) // [(i, (b ! i, True))]

parseInp :: String -> Either String Inp
parseInp s
 | s == "p" || s == "pass" || s == "resign" = Right $ InpMv Pass
 | s == "q" || s == "quit" = Right Quit
 | s == "u" || s == "undo" = Right Undo
 | s == "s" || s == "score" = Right Score
 | s == "g" || s == "go" = Right Go
 | s == "ga" || s == "goall" = Right GoAll
 | s == "" = Left "empty string?"
 | otherwise =
  let ([s1], s2) = splitAt 1 s
      xRaw = ord $ toLower s1
      x = xRaw - ord 'a' + if xRaw > ord 'i' then 0 else 1
      y = readPosInt s2 in
    case y of
      Nothing -> Left "could not parse"
      Just yy -> if yy < 1 || yy > 19 then Left "out of range"
        else Right . InpMv $ Play (x, yy)

del :: Int -> [a] -> [a]
del i = uncurry (++) . second tail . splitAt i

isComm :: Player -> Bool
isComm p = case p of
  Comm _ -> True
  _ -> False

whileM :: Monad m => m Bool -> m b -> m ()
whileM test f = test >>= \t -> when t $ f >> whileM test f

doUndo :: Int -> [Player] -> Hist -> [Comm] -> IO Hist
doUndo bdN pl hist@(_, ctx) otherComms = if ctx == PMT.Top
  then do
    print "already at beginning of game"
    return hist
  else do
    mapM_ (\ p@(inp, out, err, pid) -> do
      hPutStrLn inp "undo"
      hFlush inp
      s <- hGetLine out
      putStrLn s
      s <- hGetLine out
      putStrLn s
      whileM (hWaitForInput out 10) $ hGetLine out >>= putStrLn
      ) otherComms
    return $ fromJust $ PMT.ascend hist

sgf :: Int -> Color -> PMT.OmTree Move -> String
sgf bdN col (PMT.OmTree mOms) =
  case [";" ++ showMv m ++ sgf bdN col' om | (m, om) <- mOms] of
    [s] -> s
    ss -> concat ["(" ++ s ++ ")" | s <- ss]
  where
  mvCol = case col of
    Blk -> "B"
    Wht -> "W"
  -- todo?: capitals for over n=26 boards
  mvCoord i = chr (ord 'a' + i - 1)
  showMv Pass = mvCol ++ "[]"
  showMv (Play (x, y)) = mvCol ++ '[' : mvCoord x : mvCoord (bdN + 1 - y) : "]"
  col' = cycSucc col

histToTop :: (Ord m) => PMT.PomTree m -> PMT.PomTree m
histToTop h = case PMT.ascend h of
  Nothing -> h
  Just h' -> histToTop h'

saveGame :: GoState -> IO ()
saveGame gos = do
  let
    hist = gosHist gos
    histTop = fst $ histToTop hist
    bdN = gosBdN gos
  home <- getEnv "HOME"
  -- todo: komi etc
  -- todo: not hard-coded path lol
  writeFile (home ++ "/g/go/goa/game.sgf") $
    "(;FF[5]GM[1]SZ[" ++ show bdN ++ "]" ++ sgf bdN Blk histTop ++ ")\n"

showMove :: Move -> String
showMove (Play (x, y)) = [chr $ ord 'A' + x - if x < 9 then 1 else 0] ++ show y
showMove Pass = "pass"

rotMove :: Int -> Move -> Move
rotMove bdN (Play (x, y)) = Play (bdN + 1 - x, bdN + 1 - y)
rotMove _   m = m

playGame :: GoState ->
  ErrorT String IO (PMT.OmTree Move, PMT.OmTreeContext Move)
playGame gos = let
  dispHist = gosDisp gos
  bdN = gosBdN gos
  pl = gosPlayers gos
  comms = gosComms gos
  hist@(_, ctx) = gosHist gos
  plN = (length $ PMT.getPath hist) `mod` (length pl)
  quit = return hist
  in if isGameOver hist then quit else do
    io $ dispHist gos hist
    case pl!!plN of
      Comm n -> let
          p@(inp, out, err, pid) = comms!!n
          -- TODO: not using this?  no multicomm support
          otherComms = del n comms in do
        io $ hPutStrLn inp "genmove w"
        io $ hFlush inp
        s <- io $ hGetLine out
        --io $ putStrLn s
        s2 <- io $ hGetLine out
        --io $ putStrLn s2
        if "= " `isPrefixOf` s
          then do
            case parseInp . map toLower $ drop 2 s of
              Left err -> throwError err
              Right (InpMv move) -> do
                let
                  move' = rotMove bdN move
                io . putStrLn $ showMove move'
                -- TODO: tell other comms here
                playGame . GoState dispHist bdN pl comms $
                  PMT.descAdd move' hist
          else throwError $ "unexpected response: " ++ s
      Human -> do
        io $ saveGame gos
        mv <- io $ repInp "move: " parseInp
        case mv of
          Score -> do
            io $ mapM_ (\ p@(inp, out, err, pid) ->
              do
                hPutStrLn inp "final_score"
                hFlush inp
                s <- hGetLine out
                putStrLn s
                s <- hGetLine out
                putStrLn s
                return ()
              ) comms
            playGame $ GoState dispHist bdN pl comms hist
          Undo -> do
            hist' <- io $ doUndo bdN pl hist comms
            hist'' <- io $ doUndo bdN pl hist' comms
            playGame $ GoState dispHist bdN pl comms hist''
          Quit -> quit
          -- FIXME?: 2-pl spec, is that ok, assumes exist. of comm 0
          Go -> playGame $ GoState dispHist bdN
            ((if plN == 0 then id else reverse) [Comm 0, Human]) comms hist
          GoAll -> playGame $ GoState dispHist bdN [Comm 0, Comm 0] comms hist
          InpMv m -> (doMove m gos `catchError`
            (\ err -> io $ putStrLn err >> return gos)) >>= playGame

doMove :: Move -> GoState -> ErrorT String IO GoState
doMove mv gos = case mv of
  Play (x, y) -> if x < 1 || y < 1 || x > bdN || y > bdN
    then throwError "Move is not on board."
    else do
      ss <- io $ mapM (\ p@(inp, out, err, pid) -> do
        hPutStrLn inp $ "play b " ++ showMove (rotMove bdN $ Play (x, y))
        hFlush inp
        s1 <- hGetLine out
        putStrLn s1
        s2 <- hGetLine out
        putStrLn s2
        return s1) comms
      if "?" `isPrefixOf` head ss
        then throwError "Computer rejected the move."
        else cont
  Pass -> cont
  where
    cont = return $ gos {gosHist = PMT.descAdd mv hist}
    bdN = gosBdN gos
    comms = gosComms gos
    hist = gosHist gos

isGameOver :: PMT.PomTree Move -> Bool
isGameOver h = let p = PMT.getPath h in
  drop ((length p) - 4) p == replicate 4 Pass

-- captures
-- notice illegal moves (with gnugo?)

isEmp :: BdFill -> Bool
isEmp Emp = True
isEmp _ = False

gpLibs :: [BdPos] -> Bd -> [BdPos]
gpLibs gp b = filter (\x -> isEmp $ b ! x) . nub . concat . map (nbrs b) $ gp

nbrs :: Bd -> BdPos -> [BdPos]
nbrs b (x, y) = let
    ((xMin, yMin), (xMax, yMax)) = bounds b
    bdN = yMax - yMin + 1 in
  (if x > 1 then [(x - 1, y)] else []) ++
  (if y > 1 then [(x, y - 1)] else []) ++
  (if x < bdN then [(x + 1, y)] else []) ++
  (if y < bdN then [(x, y + 1)] else [])

getGp :: BdPos -> Bd -> [BdPos]
getGp i b = case b ! i of
  Emp -> []
  Stone c -> growGp i c b []

growGp :: BdPos -> Color -> Bd -> [BdPos] -> [BdPos]
growGp i c b gp = case b ! i of
  Stone c' -> if c' == c then if null $ filter (== i) gp
      then dlof (gp ++ [i]) $ map (\x -> growGp x c b) $ nbrs b i
      else gp
    else gp
  _ -> gp

checkDead :: BdPos -> BdState -> BdState
checkDead i bd@(b, capd) = case b ! i of
  Emp -> bd
  Stone c -> let g = getGp i b in
    if null (gpLibs g b)
       then (b // [(i, Emp) | i <- g], capd // [(c, capd ! c + length g)])
       else bd

checkKill :: BdPos -> BdState -> BdState
checkKill i bd@(b, capd) = dlof bd $ map checkDead $ nbrs b i
