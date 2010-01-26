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
import Data.Unique
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified OutdoorPainter
import qualified BasePainter
import qualified Image
import GameState
import KeyState
import Mechanics
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
    dumpState <- newIORef False
    debugState <- newIORef False
    keyState <- newIORef newKeyState

    onKeyPress window $ \Key { eventKeyName = key } -> case key of
        "Escape" -> do
            writeIORef quitState True
            return True
        "F11" -> do
            writeIORef dumpState True
            return True
        "F12" -> do
            s <- readIORef debugState
            writeIORef debugState (not s)
            putStrLn ("Debug: " ++ show (not s))
            return True
        k -> do 
            modifyIORef keyState (keyPress k)
            return True

    onKeyRelease window $ \Key { eventKeyName = key } -> case key of
        k -> do 
            modifyIORef keyState (keyRelease k)
            return True

    onDestroy window $ writeIORef quitState True

    i1 <- newUnique
    i2 <- newUnique
    let p1 = playerNew 
            (500, 200) 
            ("Up", "Down", "Left", "Right", "Shift_R", "Return")
            i1
    let p2 = playerNew 
            (200, 200) 
            ("e", "d", "s", "f", "a", "q")
            i2
    let s = GameState { 
        stateEntities = [AbstractEntity p1, AbstractEntity p2], 
        stateMap = world, 
        stateKeys = \_ -> False }

    Image.imageWithAll $ \images -> do
        newTime <- getClockTime
        mainLoop canvas backgroundSurface images quitState dumpState debugState keyState newTime s

mainLoop :: (WidgetClass widget) => widget -> Surface -> (String -> Surface) ->
    IORef Bool -> IORef Bool -> IORef Bool -> IORef KeyState -> 
    ClockTime -> GameState -> IO ()
mainLoop canvas surface images quitState dumpState debugState keyState t s = loop t s Map.empty where
    loop t s m = do
        handleEvents
        keys <- readIORef keyState
        dump <- readIORef dumpState
        debug <- readIORef debugState
        quit <- readIORef quitState
        when dump $ do
            putStrLn ("GameState at " ++ show t ++ ":")
            print s
            writeIORef dumpState False
        when (not quit) $ do
            let s' = s { stateKeys = \b -> keyPressed b keys }
            t' <- getClockTime
            let d = diffClockTime t t'
            r <- getStdRandom (first randoms . split)
            let us = map (\(e, r') -> (e, entityUpdate e s' (messages e m) r' d)) (zip (stateEntities s') r)
            let es' = concat $ map deltaEntities (map snd us)
            es'' <- forM es' $ \e -> do
                i <- newUnique
                return (e i)
            let s'' = s' { stateEntities = es'' }
            let m' = concat $ map deltaMessages (map snd us)
            let m'' = collisions es''
            let m''' = messageMap (m'' ++ m')
            renderWith surface (drawSplatter [(e, s) | (e, d) <- us, Just s <- [deltaSplatter d]])
            updateGraphics s'' canvas surface images debug
            when debug $ putStrLn ("Entity count: " ++ show (length es''))
            loop t' s'' m'''
    messages e ms = Map.findWithDefault [] (entityId e) ms
    messageMap ms = foldl (\ms' (i, m) -> Map.insert i (m:Map.findWithDefault [] i ms') ms') Map.empty ms
    collisions es =  [(entityId e1, MessageCollide e2) | 
        e1 <- es, e2 <- es,
        entityHitable e1 || entityHitable e2,
        entityId e1 /= entityId e2,
        Just (x1, y1) <- [entityPosition e1], Just r1 <- [entityRadius e1],
        Just (x2, y2) <- [entityPosition e2], Just r2 <- [entityRadius e2],
        overlap (x1, y1) r1 (x2, y2) r2]
    overlap (x1, y1) r1 (x2, y2) r2 = 
        let x = x2 - x1 in
        let y = y2 - y1 in
        let r = r1 + r2 in
        x * x + y * y < r * r

handleEvents :: IO ()
handleEvents = do
    i <- eventsPending
    if i == 0
        then return ()
        else do
            mainIteration
            handleEvents

diffClockTime :: ClockTime -> ClockTime -> Duration
diffClockTime (TOD s1 p1) (TOD s2 p2) =
    let ds = fromIntegral (s2 - s1)
        dp = fromIntegral (p2 - p1)
    in ds + (dp * 1e-12)

updateGraphics :: (WidgetClass a) => GameState -> a -> Surface -> (String -> Surface) -> Bool -> IO Bool
updateGraphics gameState canvas backgroundSurface images debug = do
    let (x, y) = (0, 0)
    (w, h) <- widgetGetSize canvas
    drawable <- widgetGetDrawWindow canvas
    drawWindowBeginPaintRect drawable (Rectangle 0 0 w h)
    renderWithDrawable drawable $ do
        setSourceSurface backgroundSurface (-x) (-y)
        paint
        let es = [(e, x, y) | e <- stateEntities gameState, Just (x, y) <- [entityPosition e]]
        mapM_ (drawEntity images) [(e, x, y) | (e, x, y) <- es, not (entityOnTop e)]
        mapM_ (drawEntity images) [(e, x, y) | (e, x, y) <- es, entityOnTop e]
        when debug $ do
            mapM_ drawDebug [(e, x, y, r) | (e, x, y) <- es, Just r <- [entityRadius e]]
    drawWindowEndPaint drawable
    return True

drawEntity :: (Entity t) => (String -> Surface) -> (t, Double, Double) -> Render ()
drawEntity images (e, x, y) = do
    save
    translate x y
    entityDraw e images
    restore

drawDebug :: (Entity t) => (t, Double, Double, Double) -> Render ()
drawDebug (e, x, y, r) = do
    save
    setSourceRGB 0 0 0
    setLineWidth 2
    arc x y r 0 (2 * pi)
    stroke
    setSourceRGB 1 0 1
    setLineWidth 1
    arc x y r 0 (2 * pi)
    stroke
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

