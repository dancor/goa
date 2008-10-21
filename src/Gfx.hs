module Gfx where

import Control.Applicative
import Control.Arrow
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Data.Array
import Go
import Paths_goa
import System.Directory
import System.Environment
import System.FilePath.Posix
import System.IO
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as Img
import qualified Graphics.UI.SDL.Rotozoomer as Rot
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
scrW :: Int
scrW = 640
scrH :: Int
scrH = 640

imgExt :: [Char]
imgExt = ".png"

imgNames :: [[Char]]
imgNames = map (++ imgExt) $ ["wood", "black", "white"] ++
  [h ++ v | h <- ["l", "m", "r"], v <- ["t", "m", "b"]] ++ ["star"]

empBd :: Array (Int, Int) (BdFill, Bool)
empBd = listArray ((1, 1), (19, 19)) $ repeat (Emp, False)

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
  SDL.blitSurface (head pics) Nothing screen . Just $ SDL.Rect 0 0 0 0
  mapM_ (\ ((x, y), (bdFill, _)) -> do
    let
      side x = if x == bdSize then 2 else case x of
        1 -> 0
        _ -> 1
      bgPic = pics !! (3 * side x + (2 - side y) + 3)
    SDL.blitSurface bgPic Nothing screen (Just $
      SDL.Rect (spotPx * x - 16) (spotPx * (bdSize - y) + 16) 0 0)
    -- fixme: n /= 19 cases
    when (x `elem` [4, 10, 15] && y `elem` [4, 10, 15]) $ do
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
  screen <- SDL.setVideoMode scrW scrH 0 []
  progName <- getProgName
  dataDir <- getDataDir
  let
    imgDir = dataDir ++ "/pics"
    scale x = (fI spotPx + 1) / fI x
  pics <- sequence . zipWith (=<<)
    (return:repeat (\ s -> Rot.zoom s (scale tileW) (scale tileH) True)) $
    map (\ x -> Img.load $ imgDir ++ "/" ++ x) imgNames
  SDL.setCaption progName ""
  return $ gm {gBdSize = bdSize, gGfx = gfx {gScreen = screen, gPics = pics}}

saveGfx :: (TVar (BdH, Array Color Int), MVar ())
           -> (BdH, Array Color Int)
           -> IO ()
saveGfx (bdV, redrawV) bd = do
  atomically $ writeTVar bdV bd
  putMVar redrawV ()

withGfx :: ((TVar (BdH, Array Color Int), MVar ())
            -> IO a)
           -> IO a
withGfx f = SDL.withInit [SDL.InitVideo] $ do
  debug "withGfx"
  bd <- atomically $ newTVar (empBd, listArray (Blk, Wht) $ repeat 0)
  redraw <- newMVar ()
  gm <- startGfx $ Game {
    gBdSize = 19,
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
