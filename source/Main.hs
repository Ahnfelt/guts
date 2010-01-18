module Main where
import Graphics.UI.Gtk hiding (fill, Solid)
import Graphics.UI.Gtk.Gdk.Events
import Graphics.Rendering.Cairo
import System.Random
import System.Time
import Control.Monad
import Data.Array.Diff
import Data.IORef
import qualified Data.Set as Set
import GameState
import PlayerEntity
import Tile
import qualified OutdoorPainter
import qualified BasePainter

ascii = [
    "*****************`   ``*************************************",
    "*****************`   `****   *    ***       **       *******",
    "*****************`````**                                ****",
    "******* **********```***                    *             **",
    "***********************       *                    *      **",
    "*************** *****               *                     **",
    "*********************    *          *    *          *      *",
    "*********************                          *           *",
    "***** *****    ****                                        *",
    "****   ***              **                       *        **",
    "***** ****                   *        *              *    **",
    "** *******     *                                         ***",
    "*   ******   *             `              *    *        ****",
    "*     **                    *            *`*       *    ****",
    "**                   *                   **              ***",
    "***              *                 *****              ******",
    "**        *                       **   *      *    ***``````",
    "*      *                *`*       *    **        **````*****",
    "*       *         *    *```*      **   **      **```**    **",
    "*             *         ***       *  ***      *`***      ***",
    "*                   *             ****     ***`*         ***",
    "**                                       **`**            **",
    "**     *                    *           **`*               *",
    "*        *      *         **             *`        *       *",
    "*       `      *                  *     *`*                *",
    "*                        *    *         *`*    `     *     *",
    "***         *`                         **`*                *",
    "****              **                   *`*                **",
    "*******    *     *****      *     *    ``*      *       ****",
    "****************************************`*******************",
    "****************************************`*******************",
    "****************************************``******************",
    "****************************************``******************",
    "****************************************`*******************",
    "***************************************``*******************"
    ]

graphicalFps = 30

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

    let p1 = Player { playerPosition = (200, 200) }
    let s = GameState { stateEntities = [entity p1], stateMap = world }

    newTime <- getClockTime
    mainLoop s canvas backgroundSurface newTime quitState keyState

mainLoop :: (WidgetClass widget) => GameState -> widget -> Surface -> ClockTime -> IORef Bool -> IORef KeyState -> IO ()
mainLoop s canvas surface oldTime quitState keyState = do
    handleEvents
    k <- readIORef keyState
    q <- readIORef quitState
    when (not q) $ do
        newTime <- getClockTime
        let d = diffClockTime oldTime newTime
        let us = map (\e -> entityUpdate e s d) (stateEntities s)
        let s' = s { stateEntities = concat $ map deltaEntities us }
        updateGraphics s' canvas surface
        mainLoop s' canvas surface newTime quitState keyState

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

