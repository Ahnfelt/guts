module Main where
import Graphics.UI.Gtk hiding (fill, Solid)
import Graphics.UI.Gtk.Gdk.Events
import Graphics.Rendering.Cairo
import Data.Array.Diff
import System.Random
import GameState
import Player
import Tile
import OutdoorPainter

ascii = [
    "************************************************************",
    "*****    ** *         ****   *    ***       **       *******",
    "****** ************  ***                                ****",
    "********    ********  **                                  **",
    "********    ***********                                   **",
    "*           *       *               *                     **",
    "*           *       *               *                      *",
    "* *******************                                      *",
    "***** *****    ****                                        *",
    "****   ***                                                **",
    "***** ****                                                **",
    "** *******                                               ***",
    "*   ******                                *             ****",
    "*     **                                 ***            ****",
    "**                   *                   **              ***",
    "***                                                      ***",
    "**                                                        **",
    "*                                                          *",
    "*                                                         **",
    "*             *                                          ***",
    "*                                                        ***",
    "**                                         **             **",
    "**                                                         *",
    "*                         *                                *",
    "*                                                          *",
    "*                                                          *",
    "***                                                        *",
    "****              **                                      **",
    "*******    *     *****      *     *    ***      *       ****",
    "************************************************************"
    ]

graphicalFps = 30

data Foo = Foo

instance Entity Foo where
    entityOnTop _ = False

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

    painters <- mapM (\f -> f $ head $ randoms $ mkStdGen 42) [roadPainter, grassPainter]

    backgroundSurface <- createImageSurface FormatRGB24 10000 10000
    renderWith backgroundSurface (drawBackground (tileMap ascii) painters)

    timeoutAdd (updateGraphics canvas backgroundSurface (0, 0)) (1000 `div` graphicalFps)
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
            mapM_ (paintTiles 42) ps
            where
                paintTiles s p = mapM_ (paintTile p) $ zip (randoms $ mkStdGen s) (tileCoordinates m)
                paintTile p (s, (x, y)) = do
                    let t = tileGet m x y
                    let ts1 = (tileGet m (x) (y - 1), tileGet m (x) (y + 1), tileGet m (x - 1) (y), tileGet m (x + 1) (y))
                    let ts2 = (tileGet m (x - 1) (y - 1), tileGet m (x + 1) (y - 1), tileGet m (x - 1) (y + 1), tileGet m (x + 1) (y + 1))
                    case p t ts1 ts2 (x * fromIntegral tileWidth) (y * fromIntegral tileWidth) s of
                        Just d -> d 
                        Nothing -> return ()
                    
                    
{-        drawGrass r x y w h = do
            let xs = randomRs (x, x + w) (mkStdGen 7)
            let ys = randomRs (y, y + h) (mkStdGen 8)
            let rs = map mkStdGen (randoms (mkStdGen 9))
            let l = take (round (w * h * 0.10)) $ zip3 rs xs ys
            let r' = mkStdGen 42
            mapM_ drawStraw l
        drawStraw (r, x, y) = do
            setLineWidth 3.0
            setLineCap LineCapRound
            setSourceRGB 0.1 0.2 0
            let (b, r') = randomR (0, 2 * pi) r
            let (e, r'') = randomR (0, 0.1 * pi + 0.5 * pi) r'
            arc (x) (y + 6) 5 b (b + e)
            stroke
            setLineWidth 2.0
            setSourceRGB 0.1 0.5 0
            arc (x) (y + 6) 5 b (b + e)
            stroke
            return r''
            -}
