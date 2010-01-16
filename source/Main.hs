module Main where
import Graphics.UI.Gtk hiding (fill, Solid)
import Graphics.UI.Gtk.Gdk.Events
import Graphics.Rendering.Cairo
import Data.Array.Diff
import System.Random
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

data Foo = Foo

instance Entity Foo where
    entityOnTop _ = False

painterGenerators = [
    OutdoorPainter.rockPainter,
    OutdoorPainter.grassPainter (\t -> t == OutdoorGrass) 0.5,
    OutdoorPainter.cloverPainter,
    BasePainter.blockPainter,
    OutdoorPainter.grassPainter (\t -> t == OutdoorBush) 0.8]
    --OutdoorPainter.flowerPainter]

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
    renderWith backgroundSurface (drawBackground world painters)

    timeoutAdd (updateGraphics canvas backgroundSurface (0, 0)) (1000 `div` graphicalFps)

    onKeyPress window $ \Key { eventKeyName = key } -> case key of
        "Escape" -> do
            mainQuit
            return True
        k -> do 
            --modifyMVar_ keyState $ \s -> return $ Set.insert k s
            return True

    onKeyRelease window $ \Key { eventKeyName = key } -> case key of
        k -> do 
            --modifyMVar_ keyState $ \s -> return $ Set.delete k s
            return True

    onDestroy window mainQuit
    mainGUI
    
    where
        updateGraphics canvas backgroundSurface (x, y) = do
            (w, h) <- widgetGetSize canvas
            drawable <- widgetGetDrawWindow canvas
            drawWindowBeginPaintRect drawable (Rectangle 0 0 w h)
            renderWithDrawable drawable $ do
                setSourceSurface backgroundSurface (-x) (-y)
                paint
            drawWindowEndPaint drawable
            return True
        drawBackground m ps = do
            setSourceRGB 0.15 0.20 0.05
            rectangle 0 0 10000 10000
            fill
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

