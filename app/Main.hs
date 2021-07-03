module Main where

import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras
import Graphics.X11.Xft
import Graphics.X11.Xrender
import System.Exit (exitWith, ExitCode(..))
import Control.Concurrent
import Control.Exception
import Control.Monad (when)
import Data.Bits ((.|.))
import System.Process (readProcess)
import System.Posix.Types (Fd(..))

data UIComponent = UIComponent {
    text  :: IO String
  , color :: IO String -- "red" or "#ff0000"
  , font  :: XftFont
  , click :: IO ()
  }

main :: IO ()
main = do
  initThreads
  xftInitFtLibrary
  dpy     <- openDisplay ""
  root    <- rootWindow dpy (defaultScreen dpy)
  win     <- makeBar dpy root 15
  font    <- xftFontOpen dpy (defaultScreenOfDisplay dpy) "scientifica"
  selectInput dpy win (exposureMask .|. buttonPressMask)
  setTextProperty dpy win "Hello World" wM_NAME
  loop dpy win [
      -- UIComponent (show <$> battery) (return "#ff0000") font (return ())
      batteryComp font
    , brighnessComp font
    , UIComponent (return "Hier" ) (return "#00ff00") font (return ())
    , UIComponent (return "ist"  ) (return "#0000ff") font (return ())
    , UIComponent (return "Luca" ) (return "#ffffff") font (return ())
    ]

-- | Main game loop. Update all the windows, poll their requests, ...
loop :: Display -> Window -> [UIComponent] -> IO ()
loop dpy win uics = do
  putStrLn "looping"
  thread <- forkIO $ sendExposeEvent dpy win
  mapWindow dpy win
  draw dpy win uics
  pollWindow dpy win [] (killThread thread)
  loop dpy win uics

-- | Make a simple window
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

-- | Make a status bar window
makeBar :: Display -> Window -> Dimension -> IO Window
makeBar dpy root height = makeWindow dpy root 0 0 width height
  where
    width = widthOfScreen (defaultScreenOfDisplay dpy)

-- | Poll all the Events the window listens to
-- The events which are listened to are set by `selectInput ...`
-- The callback is run after going through the list of events
pollWindow :: Display -> Window -> [UIComponent] -> IO () -> IO ()
pollWindow dpy win uics callback = allocaXEvent $ \e -> do
  nextEvent dpy e
  ev <- getEvent e
  when (eventName ev == "ButtonPress") (do
    (_,_,_,x,y,_,_,_,_,_) <- get_ButtonEvent e
    handleClicks dpy uics 0 0
    )
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

-- | Draw on a window
draw :: Display -> Window -> [UIComponent] -> IO ()
draw dpy win uics = do
  gc <- createGC dpy win
  mapM_ (\(u,i) -> do
    margin <- uicompSpacing dpy uics i
    drawUIComp dpy win u margin 10)
    (zip uics [0..])
  freeGC dpy gc

-- | Draw text in a window
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

drawUIComp :: Display -> Window -> UIComponent -> Int -> Int -> IO ()
drawUIComp dpy win uic x y = do
  color <- color uic
  text  <- text uic
  drawText dpy win (font uic) color x y text

handleClicks :: Display -> [UIComponent] -> Int -> Int -> IO ()
handleClicks dpy uics x y = undefined

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

urgencyLevels :: Float -> Float -> Float -> String
urgencyLevels hight low val
  | val >= hight  = "#ff0000"
  | val <= low    = "#00ffff"
  | otherwise     = "#ffff00"

uicompSpacing :: Display -> [UIComponent] -> Int -> IO Int
uicompSpacing dpy uics i =
  (+margin) . sum . map (+padding) . take i
  <$> sequence [ width dpy u | u<-uics]
  where
    padding = 5
    margin  = 10

width :: Display -> UIComponent -> IO Int
width dpy uic = do
  text <- text uic
  xglyphinfo_width <$> xftTextExtents dpy (font uic) text

-- Components

batteryComp font = UIComponent {
    text  = show . round . (*100) <$> battery
  , color = return "#eeeeee"
  , font  = font
  , click = return ()
  }

brighnessComp font = UIComponent {
    text  = slider 10 <$> brighness
  , color = return "#eeeeee"
  , font  = font
  , click = return ()
  }

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
