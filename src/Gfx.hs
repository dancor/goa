module Gfx where

import Control.Applicative
import Control.Arrow
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Data.Array
import Data.Char
import Go
import Paths_goa
import System.Directory
import System.Environment
import System.FilePath.Posix
import System.IO
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as Img
import qualified Graphics.UI.SDL.Rotozoomer as Rot
import qualified Graphics.UI.SDL.TTF as Font
import qualified PosMTree as PMT

data GfxDisp = GfxDisp
instance GameDisp GfxDisp where
  gameDisp _ gH bdN hist =
    saveGfx gH $ first (bdHilight l) (doMoves bdStInit p)
    where
    p = PMT.getPath hist
    l = if null p then Pass else last p
    bdStInit = (listArray ((1, 1), (bdN, bdN)) $ repeat Emp,
      listArray (Blk, Wht) $ repeat 0)

data Game = Game {
  gBdSize :: Int,
  gBd :: TVar (BdH, Array Color Int),
  gRedraw :: MVar (),
  gGfx :: Gfx
  }

data Gfx = Gfx {
  gScreen :: SDL.Surface,
  gPics :: [SDL.Surface],
  gSpotPx :: Int
  }

tileW :: Int
tileW = 64
tileH :: Int
tileH = 64

imgExt :: [Char]
imgExt = ".png"

imgNames :: [[Char]]
imgNames = map (++ imgExt) $ ["wood", "black", "white"] ++
  [h ++ v | h <- ["l", "m", "r"], v <- ["t", "m", "b"]] ++ ["star"]

empBd :: (Num t, Ix t, Num e) =>
         t -> (Array (t, t) (BdFill, Bool), Array Color e)
empBd bdN = (listArray ((1, 1), (bdN, bdN)) $ repeat (Emp, False),
  listArray (Blk, Wht) $ repeat 0)

debug :: [Char] -> IO ()
debug s = do
  home <- getEnv "HOME"
  let dir = home </> ".goa"
  createDirectoryIfMissing False dir
  appendFile (dir </> "debug") $ s ++ "\n"

drawBd :: Game -> IO ()
drawBd gm = do
  debug "draw"
  bd <- atomically . readTVar $ gBd gm
  let
    bdSize = gBdSize gm
    gfx = gGfx gm
    screen = gScreen gfx
    pics = gPics gfx
    spotPx = 32--gSpotPx gfx   -- wat, why..
    scrW = tileW * (bdSize + 1)
    scrH = tileH * (bdSize + 1)
  SDL.blitSurface (head pics) (Just $ SDL.Rect 0 0 scrW scrH) screen . Just $
    SDL.Rect 0 0 0 0
  mapM_ (\ ((x, y), (bdFill, _)) -> do
    let
      side x = if x == bdSize then 2 else case x of
        1 -> 0
        _ -> 1
      bgPic = pics !! (3 * side x + (2 - side y) + 3)
    SDL.blitSurface bgPic Nothing screen (Just $
      SDL.Rect (spotPx * x - 16) (spotPx * (bdSize - y) + 16) 0 0)
    -- fixme: n /= 19 cases
    when (x `elem` [4, 10, 16] && y `elem` [4, 10, 16]) $ do
      SDL.blitSurface (pics !! 12) Nothing screen (Just $
        SDL.Rect (spotPx * x - 16) (spotPx * (bdSize - y) + 16) 0 0)
      return ()
    case bdFill of
      Emp -> return ()
      Stone c -> do
        SDL.blitSurface (pics !! (fromEnum c + 1)) Nothing screen (Just $
          SDL.Rect (spotPx * x - 16) (spotPx * (bdSize - y) + 16) 0 0)
        return ()
    ) . assocs $ fst bd
  SDL.updateRect screen $ SDL.Rect 0 0 0 0
  return ()

eventLoop :: Game -> IO ()
eventLoop gm = do
  event <- SDL.waitEvent
  quit <- case event of
    SDL.VideoExpose -> do
      debug "expose"
      putMVar (gRedraw gm) ()
      return False
    SDL.KeyDown (SDL.Keysym SDL.SDLK_q _ _) -> do
      debug "q"
      return True
    SDL.Quit -> do
      debug "quit"
      return True
    _ -> return False
  unless quit $ eventLoop gm

redrawLoop :: Game -> IO ()
redrawLoop gm = do
  takeMVar $ gRedraw gm
  drawBd gm
  redrawLoop gm

fI :: Int -> Double
fI = fromIntegral

startGfx :: Game -> IO Game
startGfx gm = do
  let
    bdSize = gBdSize gm
    gfx = gGfx gm
    spotPx = gSpotPx gfx
  --SDL.showCursor False
  screen <- SDL.setVideoMode (tileW * (bdSize + 1) `div` 2)
    (tileH * (bdSize + 1) `div` 2) 0 []
  progName <- getProgName
  dataDir <- getDataDir
  let
    imgDir = dataDir ++ "/pics"
    scale x = (fI spotPx + 1) / fI x
  pics <- sequence . zipWith (=<<)
    (return:repeat (\ s -> Rot.zoom s (scale tileW) (scale tileH) True)) $
    map (\ x -> Img.load $ imgDir ++ "/" ++ x) imgNames
  -- fixme figure out something real with fonts..
  font <- Font.openFont "/usr/share/gnubg/fonts/Vera.ttf" 12
  let
    bdLtr s x1 y1 x2 y2 = do
      surf <- Font.renderTextSolid font s $ SDL.Color 0 0 0
      SDL.blitSurface surf Nothing (head pics) . Just $ SDL.Rect x1 y1 0 0
      SDL.blitSurface surf Nothing (head pics) . Just $ SDL.Rect x2 y2 0 0
    bdLabelNum i = bdLtr (show i) (xP 1) y (xP $ bdSize * tileW `div` 2 + 16) y
      where
      y = 32 * (bdSize - i + 1) - 6
      xP = if i >= 10 then id else (+ 4)
    bdLabelLtr i = bdLtr s x 1 x (bdSize * tileW `div` 2 + 18) where
      s = [adjustCh $ ord 'A' + i - 1]
      adjustCh c = chr $ if chr c >= 'I' then c + 1 else c
      x = 32 * i - 2
  mapM_ bdLabelNum [1..bdSize]
  mapM_ bdLabelLtr [1..bdSize]
  SDL.setCaption progName ""
  return $ gm {gBdSize = bdSize, gGfx = gfx {gScreen = screen, gPics = pics}}

saveGfx :: (TVar (BdH, Array Color Int), MVar ())
           -> (BdH, Array Color Int)
           -> IO ()
saveGfx (bdV, redrawV) bd = do
  atomically $ writeTVar bdV bd
  putMVar redrawV ()

withGfx :: Int
           -> ((TVar (Array (Int, Int) (BdFill, Bool), Array Color Int),
                MVar ())
               -> IO a)
           -> IO a
withGfx bdN f = SDL.withInit [SDL.InitVideo] $ do
  Font.init
  debug "withGfx"
  bd <- atomically . newTVar $ empBd bdN
  redraw <- newMVar ()
  gm <- startGfx $ Game {
    gBdSize = bdN,
    gBd = bd,
    gRedraw = redraw,
    gGfx = Gfx {
      gScreen = undefined,
      gPics = undefined,
      gSpotPx = 16
      }
    }
  forkIO $ eventLoop gm
  forkIO $ redrawLoop gm
  f (bd, redraw)
  -- fixme do we need Font.quit?
