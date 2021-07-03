module Main where

import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras
import Graphics.X11.Xft
import System.Exit (exitWith, ExitCode(..))
import Control.Concurrent
import Control.Exception
import Data.Bits ((.|.))
import System.Process (readProcess)
import System.Posix.Types (Fd(..))

main :: IO ()
main = do
  initThreads
  dpy     <- openDisplay ""
  root    <- rootWindow dpy (defaultScreen dpy)
  win     <- makeBar dpy root 15
  selectInput dpy win (exposureMask .|. buttonPressMask)
  xftInitFtLibrary
  font    <- xftFontOpen dpy (defaultScreenOfDisplay dpy) "scientifica"
  setTextProperty dpy win "Hello World" wM_NAME
  loop dpy win font

loop :: Display -> Window -> XftFont -> IO ()
loop dpy win font = do
  putStrLn "looping"
  thread <- forkIO $ sendExposeEvent dpy win
  mapWindow dpy win
  draw dpy win font
  interactWindow dpy win (killThread thread)
  loop dpy win font

makeWindow :: Display -> Window -> Position -> Position -> Dimension -> Dimension -> IO Window
makeWindow dpy root x y w h = allocaSetWindowAttributes $ \attr -> do
  initColor dpy "black" >>= set_backing_pixel attr
  set_override_redirect attr True
  createWindow
    dpy root
    x y w h 0
    depth inputOutput visual attrmask attr
      where
        border = blackPixel dpy (defaultScreen dpy)
        background = whitePixel dpy (defaultScreen dpy)
        depth = (defaultDepthOfScreen . defaultScreenOfDisplay) dpy
        attrmask = cWOverrideRedirect .|. cWBackPixel
        visual = defaultVisualOfScreen (defaultScreenOfDisplay dpy)

makeBar :: Display -> Window -> Dimension -> IO Window
makeBar dpy root height = makeWindow dpy root 0 0 width height
  where
    width = widthOfScreen (defaultScreenOfDisplay dpy)

interactWindow :: Display -> Window -> IO () -> IO ()
interactWindow dpy win callback = allocaXEvent $ \e -> do
  nextEvent dpy e
  ev <- getEvent e
  btn <- get_ButtonEvent e
  putStrLn (eventName ev)
  putStrLn (show btn)
  sync dpy True
  callback

-- | Send an expose Event which unblocks the window from polling for
-- events
sendExposeEvent :: Display -> Window -> IO ()
sendExposeEvent dpy win = do
  threadDelay (1 * 1000000)
  allocaXEvent $ \e -> do
    setEventType e expose
    sendEvent dpy win False noEventMask e
  sync dpy False

draw :: Display -> Window -> XftFont -> IO ()
draw dpy win font = do
  bgcolor <- initColor dpy "black"
  fgcolor <- initColor dpy "red"
  gc <- createGC dpy win
  setBackground dpy gc bgcolor
  setForeground dpy gc fgcolor
  -- fillRectangle dpy win gc 0 0 100 100
  -- setForeground dpy gc fgcolor
  -- fillRectangle dpy win gc 2 2 96 96
  -- drawText dpy win "scientifica" "red" 30 30 "Hier ist ein Text"
  bat <- battery
  light <- brighness
  vol <- volume
  drawText dpy win font "gray" 10 10
    (  "Battery: "++slider 10 bat
    ++ "Light: "++ slider 10 light
    ++ "Volume: "++ slider 10 vol)
  freeGC dpy gc

drawText:: Display -> Window -> XftFont -> String -> Int -> Int -> String -> IO ()
drawText dpy win font color x y text = do
  draw <- createDrawable
  withColor color (\c -> xftDrawString draw c font x y text)
  where
    colormap = defaultColormap dpy (defaultScreen dpy)
    visual = defaultVisualOfScreen (defaultScreenOfDisplay dpy)
    withColor :: String -> (XftColor -> IO a) -> IO a
    withColor = withXftColorName dpy visual colormap
    createDrawable = xftDrawCreate dpy win visual colormap

initColor :: Display -> String -> IO Pixel
initColor dpy color = do
  let colormap = defaultColormap dpy (defaultScreen dpy)
  (apros,real) <- allocNamedColor dpy colormap color
  return $ color_pixel apros

type Percentage = Float
type Celsius = Float

-- User Interface

slider :: Int -> Percentage -> String
slider width val = [ if threshold i then '#' else '·' | i<-[1..width] ]
  where
    threshold i = (fromIntegral i / fromIntegral width)<=val

wrap :: String -> String -> String -> String
wrap l r c = l ++ c ++ r

-- Sensors

battery :: IO Percentage
battery = parse <$> command
  where
    parse val = (read val :: Float) / 100
    command = readProcess "cat" ["/sys/class/power_supply/BAT0/capacity"] ""

brighness :: IO Percentage
brighness = parse <$> command
  where
    parse val = (read val :: Float) / 100
    command = readProcess "light" [] ""

volume :: IO Percentage
volume = parse <$> command
  where
    parse val = (read val :: Float) / 100
    command = readProcess "pamixer" ["--get-volume"] ""

heat :: IO [Celsius]
heat = parse <$> command
  where
    parse = map temp . cpus
    cpus val = [ line | line <- lines val, take 4 line=="Core" ]
    temp val = read $ escape $ words val !! 2
    escape :: String -> String
    escape [] = []
    escape ('+':xs) = escape xs
    escape ('-':xs) = escape xs
    escape ('\176':xs) = ""
    escape (x:xs) = x:escape xs
    command = readProcess "sensors" [] ""

ram :: IO Percentage
ram = do
  command <- readProcess "free" [] ""
  let _:total:used:free:_ = (words . (!!1) . lines) command
  return $ (read used :: Percentage) / (read total :: Percentage)
