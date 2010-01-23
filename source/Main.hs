module Main where
import Graphics.UI.Gtk hiding (fill, Solid)
import Graphics.UI.Gtk.Gdk.Events
import Graphics.Rendering.Cairo
import System.Random
import System.Time
import Control.Monad
import Control.Arrow
import Data.Array.Diff
import Data.IORef
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified OutdoorPainter
import qualified BasePainter
import GameState
import KeyState
import Message
import Tile
import PlayerEntity

ascii = let (w, h) = (100, 100) in take h $ repeat (take w $ repeat '*')

painterGenerators = [
    OutdoorPainter.rockPainter,
    OutdoorPainter.grassPainter (tileLike OutdoorGrass) 0.5,
    BasePainter.blockPainter,
    OutdoorPainter.grassPainter (tileLike OutdoorBush) 0.8]

main :: IO ()
main = do
    initGUI
    window <- windowNew
    windowFullscreen window
    set window [
        windowTitle := "Guts",
        windowDefaultWidth := 800, 
        windowDefaultHeight := 600,
        containerBorderWidth := 0]

    canvas <- drawingAreaNew
    containerAdd window canvas
    widgetShowAll window 

    painters <- mapM (\f -> f $ head $ randoms $ mkStdGen 42) painterGenerators

    let world = tileMap ascii
    backgroundSurface <- createImageSurface FormatRGB24 (tileMapWidth world * tileWidth) (tileMapHeight world * tileHeight)
    renderWith backgroundSurface (drawBackgroundTiles world painters)

    quitState <- newIORef False
    keyState <- newIORef newKeyState

    onKeyPress window $ \Key { eventKeyName = key } -> case key of
        "Escape" -> do
            writeIORef quitState True
            return True
        k -> do 
            modifyIORef keyState (keyPress k)
            return True

    onKeyRelease window $ \Key { eventKeyName = key } -> case key of
        k -> do 
            modifyIORef keyState (keyRelease k)
            return True

    onDestroy window $ writeIORef quitState True

    let p1 = playerNew 
            (500, 200) 
            ("Up", "Down", "Left", "Right", "Shift_R", "Return")
            (entityIdNew 1)
    let p2 = playerNew 
            (200, 200) 
            ("w", "s", "a", "d", "q", "1")
            (entityIdNew 2)
    let s = GameState { 
        stateEntities = [AbstractEntity p1, AbstractEntity p2], 
        stateMap = world, 
        stateKeys = \_ -> False }

    newTime <- getClockTime
    mainLoop canvas backgroundSurface quitState keyState newTime s

mainLoop :: (WidgetClass widget) => widget -> Surface -> IORef Bool -> IORef KeyState -> ClockTime -> GameState -> IO ()
mainLoop canvas surface quitState keyState t s = loop t s Map.empty 1000 where
    loop t s m i = do
        handleEvents
        k <- readIORef keyState
        q <- readIORef quitState
        when (not q) $ do
            let s' = s { stateKeys = \b -> keyPressed b k }
            t' <- getClockTime
            let d = diffClockTime t t'
            r <- getStdRandom (first randoms . split)
            let us = map (\(e, r') -> (e, entityUpdate e s' (messages e m) r' d)) (zip (stateEntities s') r)
            let es' = concat $ map deltaEntities (map snd us)
            let es'' = map (\(i, e) -> e (entityIdNew i)) (zip [i..] es')
            print (length es'')
            let s'' = s' { stateEntities = es'' }
            let m' = concat $ map deltaMessages (map snd us)
            let m'' = [(entityId e1, MessageCollide e2) | 
                    e1 <- es'', e2 <- es'', 
                    entityHitable e1 || entityHitable e2,
                    entityId e1 /= entityId e2,
                    Just p1 <- [entityPosition e1], Just b1 <- [entityBox e1],
                    Just p2 <- [entityPosition e2], Just b2 <- [entityBox e2],
                    overlap (p1, b2) (p2, b2)]
            let m''' = messageMap (m'' ++ m')
            renderWith surface (drawSplatter [(e, s) | (e, d) <- us, Just s <- [deltaSplatter d]])
            updateGraphics s'' canvas surface
            loop t' s'' m''' (i + fromIntegral (length es'))
    messages e ms = Map.findWithDefault [] (entityId e) ms
    messageMap ms = foldl (\ms' (i, m) -> Map.insert i (m:Map.findWithDefault [] i ms') ms') Map.empty ms
    overlap ((x1, y1), (w1, h1)) ((x2, y2), (w2, h2)) = 
        intersectRange (x1, x1 + w1 - 1) (x2, x2 + w2 - 1) && intersectRange (y1, y1 + h1 - 1) (y2, y2 + h2 - 1)
        where
            intersectRange (x1, x2) (x1', x2') | x1 <= x1' = x2 >= x1'
            intersectRange c c' = intersectRange c' c

handleEvents :: IO ()
handleEvents = do
    i <- eventsPending
    if i == 0
        then return ()
        else do
            mainIteration
            handleEvents

diffClockTime :: ClockTime -> ClockTime -> Double
diffClockTime (TOD s1 p1) (TOD s2 p2) =
    let ds = fromIntegral (s2 - s1)
        dp = fromIntegral (p2 - p1)
    in ds + (dp * 1e-12)

updateGraphics :: (WidgetClass a) => GameState -> a -> Surface -> IO Bool
updateGraphics gameState canvas backgroundSurface = do
    let (x, y) = (0, 0)
    (w, h) <- widgetGetSize canvas
    drawable <- widgetGetDrawWindow canvas
    drawWindowBeginPaintRect drawable (Rectangle 0 0 w h)
    renderWithDrawable drawable $ do
        setSourceSurface backgroundSurface (-x) (-y)
        paint
        mapM_ drawEntity [(e, x, y) | e <- stateEntities gameState, Just (x, y) <- [entityPosition e], not (entityOnTop e)]
        mapM_ drawEntity [(e, x, y) | e <- stateEntities gameState, Just (x, y) <- [entityPosition e], entityOnTop e]
    drawWindowEndPaint drawable
    return True

drawEntity :: (Entity t) => (t, Double, Double) -> Render ()
drawEntity (e, x, y) = do
    save
    translate x y
    entityDraw e
    restore

drawSplatter :: (Entity t) => [(t, Render ())] -> Render ()
drawSplatter ps = do
    forM_ ps $ \(e, r) -> do
        case entityPosition e of
            Just (x, y) -> do
                save
                translate x y
                r
                restore
            Nothing -> return ()

drawBackgroundTiles :: TileMap -> [TilePainter] -> Render ()
drawBackgroundTiles m ps = do
    mapM_ (paintTiles 7) ps
    where
        paintTiles s p = mapM_ (paintTile p) $ zip (randoms $ mkStdGen s) (tileCoordinates m)
        paintTile p (s, (x, y)) = do
            let t = tileGet m x y
            let ts1 = (tileGet m (x) (y - 1), tileGet m (x) (y + 1), tileGet m (x - 1) (y), tileGet m (x + 1) (y))
            let ts2 = (tileGet m (x - 1) (y - 1), tileGet m (x + 1) (y - 1), tileGet m (x - 1) (y + 1), tileGet m (x + 1) (y + 1))
            save
            p t ts1 ts2 (x * fromIntegral tileWidth) (y * fromIntegral tileWidth) s
            restore

