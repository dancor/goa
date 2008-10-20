-- wrapper to gnugo

-- unicode-text front-end
-- sdl-graphics front-end
-- happs?-net front-end

module Main where

import qualified AnsiColor as C
import qualified Control.Arrow as CA
import qualified Control.Monad as CM
import qualified Data.Array as Arr
import qualified Data.Char as Chr
import qualified Data.List as Lst
import qualified Data.Maybe as Mby
import qualified Paths_goa as Paths
import qualified System.Console.GetOpt as Opt
import qualified System.Environment as Env
import qualified System.IO as SIO
import qualified System.Process as SP

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as Img

data Game = Game {
  gBdSize :: Int,
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

drawBd :: Gfx -> IO ()
drawBd gfx = do
  let
    screen = gScreen gfx
    pics = gPics gfx
  SDL.fillRect screen Nothing (SDL.Pixel 0)
  SDL.blitSurface (head pics) Nothing screen . Just $ SDL.Rect 0 0 0 0
  SDL.updateRect screen $ SDL.Rect 0 0 0 0
  return ()

mainLoop :: Gfx -> IO ()
mainLoop s = do
  event <- SDL.waitEvent
  quit <- case event of
    SDL.VideoExpose -> drawBd s >> return False
    SDL.KeyDown (SDL.Keysym SDL.SDLK_q _ _) -> return True
    SDL.Quit -> return True
    _ -> return False
  CM.unless quit $ mainLoop s

startGfx :: Game -> IO Game
startGfx gm = do
  let
    bdSize = gBdSize gm
    gfx = gGfx gm
    spotPx = gSpotPx gfx
  --SDL.showCursor False
  screen <- SDL.setVideoMode scrW scrH 0 []
  progName <- Env.getProgName
  dataDir <- Paths.getDataDir
  let imgDir = dataDir ++ "/pics"
  pics <- mapM Img.load $ map (\x -> imgDir ++ "/" ++ x) imgNames
  SDL.setCaption progName ""
  return $ gm {gBdSize = bdSize, gGfx = gfx {gScreen = screen, gPics = pics}}

main = SDL.withInit [SDL.InitVideo] $ do
  g <- startGfx $ Game {
    gBdSize = 19,
    gGfx = Gfx {
      gScreen = undefined,
      gPics = undefined,
      gSpotPx = 16
      }
    }
  let
    s = gGfx g
  drawBd s
  mainLoop s
