module Main where
import Graphics.UI.Gtk hiding (fill, Solid)
import Graphics.UI.Gtk.Gdk.Events
import Graphics.Rendering.Cairo
import System.Random
import System.Time
import Control.Monad
import Control.Arrow
import Data.Array
import Data.IORef
import Data.Unique
import Data.Maybe
import Data.List (sortBy)
import Data.Ord (comparing)
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Image
import qualified Painter.Outdoor
import qualified Painter.Base
import KeyState
import Message
import Layer
import World.Mechanics
import World.Tile
import World.Barrier
import Entity
import Entity.Player

--ascii = let (w, h) = (100, 100) in take h $ repeat (take w $ repeat '*')

ascii = [
    "************************************************************",
    "*                                                          *",
    "*                                                          *",
    "*                                                          *",
    "*                                                          *",
    "*           `             `                                *",
    "*                   bbbbbbbbb                              *",
    "*                ` `b```````b                              *",
    "*               ` ```````*``b                              *",
    "*                bbbbbb`````b                              *",
    "*                b````b`````b      ****                    *",
    "*              ` b`bb`b`````b     *** *                    *",
    "*               `b``b`b``bb`b       ****                   *",
    "*              b`b`bb`b``b``b         *                    *",
    "*          `   b```b`````b`bb                              *",
    "*             bbbb`b`bbbbb`b                               *",
    "*                b`````b```b            ```                *",
    "*                bbbbb`b`bbb       ```` `` ``              *",
    "*           *        `````       ```````` ``               *",
    "*                     ` `      ````   ``                   *",
    "*                                `     `                   *",
    "*                                                          *",
    "*                      `                                   *",
    "*            *                                             *",
    "*                                         `                *",
    "*                        ** **                             *",
    "*                      ***   ***                           *",
    "*                     **       **                          *",
    "*                   ***         ***                        *",
    "************************************************************"
    ]

painterGenerators = [
    Painter.Outdoor.rockPainter,
    Painter.Outdoor.grassPainter (tileLike OutdoorGrass) 0.5,
    Painter.Outdoor.grassPainter (tileLike OutdoorBush) 1.0,
    Painter.Base.blockPainter]

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
    let (worldWidth, worldHeight) = (tileMapWidth world * tileWidth, tileMapHeight world * tileHeight)
    backgroundSurface <- createImageSurface FormatRGB24 worldWidth worldHeight
    renderWith backgroundSurface (drawBackgroundTiles world painters)

    quitState <- newIORef False
    debugState <- newIORef False
    keyState <- newIORef newKeyState

    onKeyPress window $ \Key { eventKeyName = key } -> case key of
        "Escape" -> do
            writeIORef quitState True
            return True
        "F12" -> do
            s <- readIORef debugState
            writeIORef debugState (not s)
            putStrLn ("Debug: " ++ show (not s))
            return True
        k -> do 
            modifyIORef keyState (keyPress k)
            return True

    onKeyRelease window $ \Key { eventKeyName = k } -> do
        modifyIORef keyState (keyRelease k)
        return True

    onDestroy window $ writeIORef quitState True

    i1 <- newUnique
    i2 <- newUnique
    let p1 = playerNew 
            (500, 200) 
            ["Up", "Down", "Left", "Right", "Shift_R", "Return"]
            i1
    let p2 = playerNew 
            (200, 200) 
            ["e", "d", "s", "f", "a", "q"]
            i2
    let s = GameState { 
        stateEntities = [AbstractEntity p1, AbstractEntity p2], 
        stateMap = world, 
        stateKeys = \_ -> False,
        stateBarriers = addBarrier (foldl (addBarrier) (newBarrierMap worldWidth worldHeight) (solidEdges world)) ((200, 0), (350, 400))}
    Image.imageWithAll $ \images -> do
        newTime <- getClockTime
        mainLoop canvas backgroundSurface images quitState debugState keyState newTime s

mainLoop :: (WidgetClass widget) => widget -> Surface -> (String -> Surface) ->
    IORef Bool -> IORef Bool -> IORef KeyState -> 
    ClockTime -> GameState -> IO ()
mainLoop canvas surface images quitState debugState keyState t s = loop t s Map.empty where
    loop t s m = do
        handleEvents
        keys <- readIORef keyState
        debug <- readIORef debugState
        quit <- readIORef quitState
        when (not quit) $ do
            let s' = s { stateKeys = \b -> keyPressed b keys }
            t' <- getClockTime
            let d = diffClockTime t t'
            r <- getStdRandom (first randoms . split)
            let us = map (\(e, r') -> (e, updateEntity e s' r' {- (messages e m) r' d-})) (zip (stateEntities s') r)
            let es' = concat $ map deltaEntities (map snd us)
            es'' <- forM es' $ \e -> do
                i <- newUnique
                return (e i)
            let s'' = s' { stateEntities = es'' }
            let m' = concat $ map (\e -> [(e', (deltaSelf e, m)) | (e', m) <- deltaMessages e]) (map snd us)
            let m'' = collisions es''
            let m''' = messageMap (m'' ++ m')
            renderWith surface (sequence_ [s | (_, d) <- us, Just s <- [deltaSplatter d]])
            updateGraphics s'' canvas surface images [] debug
            --when debug $ putStrLn ("Entity count: " ++ show (length es''))
            loop t' s'' m'''
    messages e ms = [] -- Map.findWithDefault [] (entityId e) ms
    messageMap ms = foldl (\ms' (i, m) -> Map.insert i (m:Map.findWithDefault [] i ms') ms') Map.empty ms
    collisions es = [] {- [(entityId e2, (e1, MessageCollide)) | 
        e1 <- filter entityHitable es, e2 <- es,
        entityHitable e1 || entityHitable e2,
        entityId e1 /= entityId e2,
        Just (x1, y1) <- [entityPosition e1], Just r1 <- [entityRadius e1],
        Just (x2, y2) <- [entityPosition e2], Just r2 <- [entityRadius e2],
        overlap (x1, y1) r1 (x2, y2) r2]
    overlap (x1, y1) r1 (x2, y2) r2 = 
        let x = x2 - x1 in
        let y = y2 - y1 in
        let r = r1 + r2 in
        x * x + y * y < r * r -}

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

updateGraphics :: (WidgetClass a) => GameState -> a -> Surface -> (String -> Surface) -> [(Layer, Render ())] -> Bool -> IO Bool
updateGraphics gameState canvas backgroundSurface images drawings debug = do
    let (x, y) = (0, 0)
    (w, h) <- widgetGetSize canvas
    drawable <- widgetGetDrawWindow canvas
    drawWindowBeginPaintRect drawable (Rectangle 0 0 w h)
    renderWithDrawable drawable $ do
        save
        translate (-x) (-y)
        setSourceSurface backgroundSurface 0 0
        paint
        mapM_ snd (sortBy (comparing fst) drawings)
        when debug $ do
            mapM_ drawDebugBarrier (Set.toList $ Set.fromList (barriersOverlapping (stateBarriers gameState) (x, y) (x + fromIntegral w, y + fromIntegral h)))
        restore
    drawWindowEndPaint drawable
    return True

drawDebugBarrier :: LineSegment -> Render ()
drawDebugBarrier ((x1, y1), (x2, y2)) = do
    save
    setSourceRGB 0 0 0
    setLineWidth 2
    moveTo x1 y1
    lineTo x2 y2
    stroke
    setSourceRGB 1 0 1
    setLineWidth 1
    moveTo x1 y1
    lineTo x2 y2
    stroke
    setSourceRGB 1 0 1
    arc x1 y1 2 0 (2 * pi)
    fill
    setSourceRGB 1 0 1
    arc x2 y2 2 0 (2 * pi)
    fill
    restore

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


