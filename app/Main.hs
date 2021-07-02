module Main where

import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras
import Graphics.X11.Xft
import System.Exit (exitWith, ExitCode(..))
import Control.Concurrent
import Control.Exception
import Data.Bits ((.|.))

main :: IO ()
main = do
  display <- openDisplay ""
  root    <- rootWindow display (defaultScreen display)
  win     <- makeBar display root 15
  setTextProperty display win "Hello World" wM_NAME
  loop display win

loop :: Display -> Window -> IO ()
loop display win = do
  threadDelay (1 * 1000000)
  putStrLn "looping"
  mapWindow display win
  drawInWin display win
  loop display win

makeWindow :: Display -> Window -> Position -> Position -> Dimension -> Dimension -> IO Window
makeWindow display root x y w h = allocaSetWindowAttributes $ \attr -> do
  initColor display "black" >>= set_backing_pixel attr
  set_override_redirect attr True
  createWindow
    display root
    x y w h 0
    depth inputOutput visual attrmask attr
      where
        border = blackPixel display (defaultScreen display)
        background = whitePixel display (defaultScreen display)
        depth = (defaultDepthOfScreen . defaultScreenOfDisplay) display
        attrmask = cWOverrideRedirect .|. cWBackPixel
        visual = defaultVisualOfScreen (defaultScreenOfDisplay display)

makeBar :: Display -> Window -> Dimension -> IO Window
makeBar display root height = makeWindow display root 0 0 width height
  where
    width = widthOfScreen (defaultScreenOfDisplay display)

drawInWin :: Display -> Window -> IO ()
drawInWin dpy win = do
  bgcolor <- initColor dpy "black"
  fgcolor <- initColor dpy "white"
  gc <- createGC dpy win
  setBackground dpy gc bgcolor
  setForeground dpy gc fgcolor
  -- fillRectangle dpy win gc 0 0 100 100
  -- setForeground dpy gc fgcolor
  -- fillRectangle dpy win gc 2 2 96 96
  drawText dpy win "scientifica" "red" 30 30 "Hier ist ein Text"
  freeGC dpy gc

drawText:: Display -> Window -> String -> String -> Int -> Int -> String -> IO ()
drawText dpy win fontName color x y text = do
  font <- loadFont fontName
  draw <- createDrawable
  withColor color (\c -> xftDrawString draw c font x y text)
  where
    colormap = defaultColormap dpy (defaultScreen dpy)
    visual = defaultVisualOfScreen (defaultScreenOfDisplay dpy)
    withColor :: String -> (XftColor -> IO a) -> IO a
    withColor = withXftColorName dpy visual colormap
    loadFont = xftFontOpen dpy (defaultScreenOfDisplay dpy)
    createDrawable = xftDrawCreate dpy win visual colormap

initColor :: Display -> String -> IO Pixel
initColor dpy color = do
  let colormap = defaultColormap dpy (defaultScreen dpy)
  (apros,real) <- allocNamedColor dpy colormap color
  return $ color_pixel apros
