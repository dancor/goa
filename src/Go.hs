module Go where

import Control.Arrow
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
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
import qualified PosMTree as PMT

type BdPos = (Int, Int)
type Comm = (Handle, Handle, Handle, ProcessHandle)
data Player = Human | Comm Int
data Move = Pass | Play BdPos deriving (Show, Ord, Eq)
data Inp = InpMv Move | Undo | Score | Go | GoAll | Quit
data Color = Blk | Wht deriving (Eq, Bounded, Enum, Ord, Ix, Show)
data BdFill = Emp | Stone Color deriving (Eq, Show)
type Bd = Array BdPos BdFill
type BdH = Array BdPos (BdFill, Bool)
type BdState = (Bd, Array Color Int)
type Hist = PMT.PosMTree Move

class GameDisp a where
  gameDisp :: a -> (TVar (BdH, Array Color Int), MVar ()) -> Int ->
    PMT.PosMTree Move -> IO ()

data GoState = GoState {
  gosDisp :: Int -> (PMT.MTree Move, PMT.MTreeContext Move) -> IO (),
  gosBdN :: Int,
  gosPlayers :: [Player],
  gosComms :: [(Handle, Handle, Handle, ProcessHandle)],
  gosHist :: PMT.PosMTree Move
  }

readPosInt :: String -> Maybe Int
readPosInt s = if not (null s) && all isDigit s then Just $ read s else Nothing

bdHilight :: Move -> Bd -> BdH
bdHilight m b = case m of
  Pass -> fmap (\p -> (p, False)) b
  Play i -> (fmap (\p -> (p, False)) b) // [(i, (b ! i, True))]

parseInp :: String -> Either String Inp
parseInp s
 | s == "p" || s == "pass" = Right $ InpMv Pass
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

      {-
      hPutStrLn inp "showboard"
      hFlush inp
      -}
      {-
      hWaitForInput out 10
      whileM (hReady out) $ hGetLine out >>= putStrLn
      -}
      whileM (hWaitForInput out 10) $ hGetLine out >>= putStrLn
      ) otherComms
    return $ fromJust $ PMT.ascend hist

doTurn :: GoState -> IO (Either String (PMT.MTree Move, PMT.MTreeContext Move))
doTurn gos = let
  dispHist = gosDisp gos
  bdN = gosBdN gos
  pl = gosPlayers gos
  comms = gosComms gos
  hist@(_, ctx) = gosHist gos
  plN = (length $ PMT.getPath hist) `mod` (length pl)
  quit = return $ Right hist
  in if isGameOver hist then quit else do
    dispHist bdN hist
    case pl!!plN of
      Comm n -> let
          p@(inp, out, err, pid) = comms!!n
          -- TODO: not using this?  no multicomm support
          otherComms = del n comms in do
        hPutStrLn inp "genmove w"
        hFlush inp
        s <- hGetLine out
        putStrLn s
        s2 <- hGetLine out
        putStrLn s2
        if "= " `isPrefixOf` s
          then do
            case parseInp . map toLower $ drop 2 s of
              Left err -> do
                return $ Left err
              Right (InpMv move) -> do
                --putStrLn $ show move
                -- TODO: tell other comms here
                doTurn $ GoState dispHist bdN pl comms (PMT.descAdd move hist)
          else return . Left $ "unexpected response: " ++ s
      Human -> do
        mv <- repInp "move: " parseInp
        case mv of
          Score -> do
            mapM_ (\ p@(inp, out, err, pid) ->
              do
                hPutStrLn inp "final_score"
                hFlush inp
                s <- hGetLine out
                putStrLn s
                s <- hGetLine out
                putStrLn s
                return ()
              ) comms
            doTurn $ GoState dispHist bdN pl comms hist
          Undo -> do
            hist' <- doUndo bdN pl hist comms
            hist'' <- doUndo bdN pl hist' comms
            doTurn $ GoState dispHist bdN pl comms hist''
          Quit -> quit
          -- FIXME?: 2-pl spec, is that ok, assumes exist. of comm 0
          Go -> doTurn $ GoState dispHist bdN ((if plN == 0 then id else reverse)
            [Comm 0, Human]) comms hist
          GoAll -> doTurn $ GoState dispHist bdN [Comm 0, Comm 0] comms hist
          InpMv m -> doMove m gos >>= \ r -> case r of
            Left err -> putStrLn err >> doTurn gos
            Right gos' -> doTurn gos'

doMove :: Move -> GoState -> IO (Either String GoState)
doMove mv gos = case mv of
  Play (x, y) -> if x < 1 || y < 1 || x > bdN || y > bdN
    then return $ Left "Move is not on board."
    else do
      ss <- mapM (\ p@(inp, out, err, pid) -> do
        hPutStrLn inp $ "play b " ++
          [chr $ ord 'a' + x - if x < 9 then 1 else 0] ++
          show y
        hFlush inp
        s1 <- hGetLine out
        putStrLn s1
        s2 <- hGetLine out
        putStrLn s2
        return s1) comms
      if "?" `isPrefixOf` head ss
        then return $ Left "Engine fail."
        else cont
  Pass -> cont
  where
    cont = return . Right $ gos {gosHist = PMT.descAdd mv hist}
    bdN = gosBdN gos
    comms = gosComms gos
    hist = gosHist gos

isGameOver :: PMT.PosMTree Move -> Bool
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
