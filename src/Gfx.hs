module Gfx where

import Control.Concurrent
import Control.Monad
import Data.Array
import Paths_goa
import System.Environment
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as Img
import qualified Graphics.UI.SDL.Rotozoomer as Rot

data Game = Game {
  gBdSize :: Int,
  gBd :: Array (Int, Int) Int,  -- 0 Empty, 1 Black, 2 White
  gGfx :: Gfx
  }

data Gfx = Gfx {
  gScreen :: SDL.Surface,
  gPics :: [SDL.Surface],
  gSpotPx :: Int
  }

tileW = 64
tileH = 64
scrW = 640
scrH = 640

imgExt = ".png"
imgNames = map (++ imgExt) $ ["wood", "black", "white"] ++
  [h ++ v | h <- ["l", "m", "r"], v <- ["t", "m", "b"]]

drawBd :: Game -> IO ()
drawBd gm = do
  let
    bd = gBd gm
    bdSize = gBdSize gm
    gfx = gGfx gm
    screen = gScreen gfx
    pics = gPics gfx
    spotPx = 32--gSpotPx gfx   -- wat, why..
  SDL.blitSurface (head pics) Nothing screen . Just $ SDL.Rect 0 0 0 0
  mapM_ (\ ((x, y), i) -> do
    let
      side x = if x == bdSize then 2 else case x of
        1 -> 0
        _ -> 1
      bgPic = pics !! (3 * side x + side y + 3)
    SDL.blitSurface bgPic Nothing screen (Just $
      SDL.Rect (spotPx * x - 16) (spotPx * y - 16) 0 0)
    when (i > 0) $ SDL.blitSurface (pics !! i) Nothing screen (Just $
      SDL.Rect (spotPx * x - 16) (spotPx * y - 16) 0 0) >> return ()
    ) $ assocs bd
  SDL.updateRect screen $ SDL.Rect 0 0 0 0
  return ()

mainLoop :: Game -> IO ()
mainLoop gm = do
  event <- SDL.waitEvent
  quit <- case event of
    SDL.VideoExpose -> drawBd gm >> return False
    SDL.KeyDown (SDL.Keysym SDL.SDLK_q _ _) -> return True
    SDL.Quit -> return True
    _ -> return False
  unless quit $ mainLoop gm

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

dispHist _ _ _ = return ()

withGfx f = SDL.withInit [SDL.InitVideo] $ do
  gm <- startGfx $ Game {
    gBdSize = 19,
    gBd = listArray ((1, 1), (19, 19)) $ repeat 0,
    gGfx = Gfx {
      gScreen = undefined,
      gPics = undefined,
      gSpotPx = 16
      }
    }
  drawBd gm
  forkIO $ mainLoop gm
  f
