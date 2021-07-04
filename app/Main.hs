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
import System.Process (readProcess, readProcessWithExitCode, spawnProcess)
import System.Posix.Types (Fd(..))

data UIComponent = UIComponent {
    text  :: IO String
  , color :: IO String -- "red" or "#ff0000"
  , font  :: XftFont
  , click :: Percentage -> Percentage -> IO ()
  }

main :: IO ()
main = do
  initThreads
  xftInitFtLibrary
  dpy     <- openDisplay ""
  root    <- rootWindow dpy (defaultScreen dpy)
  win     <- makeBar dpy root 15
  font    <- xftFontOpen dpy (defaultScreenOfDisplay dpy) "scientifica:size=9:antialias=false"
  selectInput dpy win (exposureMask .|. buttonPressMask)
  setTextProperty dpy win "Hello World" wM_NAME
  loop dpy win
    [
      labelComp font "Bg: "
    , brighnessComp font
    ]
    [
      labelComp font "Bat: "
    , batteryComp font
    , spacing font
    , labelComp font "Ram: "
    , ramComp font
    , spacing font
    , labelComp font "Bg: "
    , brighnessComp font
    , spacing font
    , labelComp font "Vol: "
    , volumeComp font
    , spacing font
    , labelComp font "Heat: "
    , heatComp font
    , spacing font
    , timeComp font
    ]

-- | Main game loop. Update all the windows, poll their requests, ...
loop :: Display -> Window -> [UIComponent] -> [UIComponent] -> IO ()
loop dpy win uicsl uicsr = do
  putStrLn "looping"
  thread <- forkIO $ sendExposeEvent dpy win
  mapWindow dpy win
  draw dpy win uicsl uicsr
  pollWindow dpy win uicsl uicsr (do
      killThread thread
      draw dpy win uicsl uicsr
    )
  loop dpy win uicsl uicsr

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
pollWindow :: Display -> Window -> [UIComponent] -> [UIComponent] -> IO () -> IO ()
pollWindow dpy win uicsl uicsr callback = allocaXEvent $ \e -> do
  nextEvent dpy e
  ev <- getEvent e
  when (eventName ev == "ButtonPress") (do
    (_,_,_,x,y,_,_,_,_,_) <- get_ButtonEvent e
    handleClicks dpy uicsl uicsr (fromIntegral x) (fromIntegral y)
    )
  sync dpy True
  callback

-- | Send an expose Event which unblocks the window from polling for
-- events
sendExposeEvent :: Display -> Window -> IO ()
sendExposeEvent dpy win = do
  threadDelay (3 * 1000000)
  allocaXEvent $ \e -> do
    setEventType e expose
    sendEvent dpy win False noEventMask e
  sync dpy False

-- | Draw on a window
draw :: Display -> Window -> [UIComponent] -> [UIComponent] -> IO ()
draw dpy win uicsl uicsr = do
  forEachUIComp dpy uicsl (\dist _ u -> drawUIComp dpy win u dist 10)
  forEachUIComp dpy (reverse uicsr) (\dist w u -> drawUIComp dpy win u (distanceFromRight dist w) 10)
  return ()
    where
      distanceFromRight dist w = fromIntegral screenWidth - dist - w
      screenWidth = widthOfScreen (defaultScreenOfDisplay dpy)

-- | Draw text in a window
drawText:: Display -> Window -> XftFont -> String -> Int -> Int -> String -> IO ()
drawText dpy win font color x y text = do
  draw <- createDrawable
  withColor color (\c -> xftDrawString draw c font x y text)
  xftDrawDestroy draw
  where
    colormap = defaultColormap dpy (defaultScreen dpy)
    visual = defaultVisualOfScreen (defaultScreenOfDisplay dpy)
    createDrawable = xftDrawCreate dpy win visual colormap
    withColor :: String -> (XftColor -> IO a) -> IO a
    withColor = withXftColorName dpy visual colormap

drawUIComp :: Display -> Window -> UIComponent -> Int -> Int -> IO ()
drawUIComp dpy win uic x y = do
  color <- color uic
  text  <- text uic
  widt <- width dpy uic
  heig <- height dpy uic
  clearArea dpy win
    (fromIntegral x)
    (fromIntegral (y-8))
    (fromIntegral widt)
    (fromIntegral heig)
    False
  drawText dpy win (font uic) color x y text

-- | Execute some function for each ui component
-- Also returns the total width of the text generated
forEachUIComp :: Display -> [UIComponent] -> (Int -> Int -> UIComponent -> IO ()) -> IO Int
forEachUIComp dpy uics func = foldl iterator (return margin :: IO Int) uics
  where
    iterator d u = do
      dist <- d
      widt <- width dpy u
      func dist widt u
      return (dist + widt + padding)

handleClicks :: Display -> [UIComponent] -> [UIComponent] -> Int -> Int -> IO ()
handleClicks dpy uicsl uicsr x y = do
  forEachUIComp dpy uicsl (activateIfInside x y)
  w <- forEachUIComp dpy uicsr (\ _ _ _ -> return ())
  forEachUIComp dpy uicsr (activateIfInside (x-screenWidth+w) y)
  return ()
  where
    screenWidth = fromIntegral $ widthOfScreen (defaultScreenOfDisplay dpy)
    activateIfInside x y d widt u = do
      heig <- height dpy u
      let ins = inside x y d 0 widt heig
      let px = fromIntegral (x-d)/fromIntegral widt
      let py = fromIntegral y/fromIntegral heig
      when ins (click u px py)
    inside x y dx dy w h = x>=dx && x<=dx+w && y>=dy && y <=dy+h

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

padding = 5
margin  = 10

urgencyLevels :: Float -> Float -> Float -> String
urgencyLevels critical warning val
  | val >= critical = "darkred"
  | val >= warning  = "darkorange"
  | otherwise       = "darkgreen"

width :: Display -> UIComponent -> IO Int
width dpy uic = do
  text <- text uic
  xglyphinfo_width <$> xftTextExtents dpy (font uic) text

height :: Display -> UIComponent -> IO Int
height dpy uic = do
  text <- text uic
  xglyphinfo_height <$> xftTextExtents dpy (font uic) text

-- Components

spacing font = UIComponent {
    text  = return "|"
  , color = return "#646464"
  , font  = font
  , click = \px py -> return ()
  }

labelComp font label = UIComponent {
    text  = return label
  , color = return "#646464"
  , font  = font
  , click = \px py -> return ()
  }

batteryComp font = UIComponent {
    text  = show . round . (*100) <$> battery
  , color = return "#eeeeee"
  , font  = font
  , click = \px py -> return ()
  }

brighnessComp font = UIComponent {
    text  = slider 10 <$> brighness
  , color = return "#eeeeee"
  , font  = font
  , click = \px py -> do
    spawnProcess "sudo" ["light", "-S", show . (*100) $ px]
    return ()
  }

volumeComp font = UIComponent {
    text  = slider 10 <$> volume
  , color = return "#eeeeee"
  , font  = font
  , click = \px py -> do
    spawnProcess "pamixer" ["--set-volume", show . round . (*100) $ px]
    print "change volume"
    return ()
  }

heatComp font = UIComponent {
    text  = show <$> heat
  , color = urgencyLevels 75 55 . maximum <$> heat
  , font  = font
  , click = \px py -> return ()
  }

timeComp font = UIComponent {
    text  = time
  , color = return "#ababab"
  , font  = font
  , click = \px py -> do
    spawnProcess "xdg-open" ["https://calendar.google.com/"]
    return ()
  }

ramComp font = UIComponent {
    text  = (++"%") . show . round . (*100) <$> ram
  , color = return "#eeeeee"
  , font  = font
  , click = \px py -> return ()
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
    command = do
      (_,val,_) <- readProcessWithExitCode "pamixer" ["--get-volume"] ""
      return val

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

time :: IO String
time = head . lines <$> readProcess "date" ["+%d.%m.%Y %H:%M"] ""
