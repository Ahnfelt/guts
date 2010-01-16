module Main where
import Graphics.UI.Gtk hiding (fill, Solid)
import Graphics.UI.Gtk.Gdk.Events
import Graphics.Rendering.Cairo
import System.Random
import Control.Concurrent
import Control.Monad
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Data.Array.Diff
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
    renderWith backgroundSurface (drawBackground world painters)

    keyState <- newTVarIO newKeyState

    onKeyPress window $ \Key { eventKeyName = key } -> case key of
        "Escape" -> do
            mainQuit
            return True
        k -> do 
            modifyTVar (keyPress k) keyState
            return True

    onKeyRelease window $ \Key { eventKeyName = key } -> case key of
        k -> do 
            modifyTVar (keyRelease k) keyState
            return True

    onDestroy window mainQuit

    let p1 = Player { playerPosition = (200, 200) }
    gameState <- newTVarIO (GameState { stateEntities = [entity p1], stateMap = world })
    threadId <- forkIO (gameLoop gameState keyState)
    timeoutAdd (updateGraphics gameState canvas backgroundSurface) (1000 `div` graphicalFps)

    mainGUI

    killThread threadId
    
    where
        modifyTVar f v = atomically $ do
            s <- readTVar v
            writeTVar v (f s)
        updateGraphics gameState canvas backgroundSurface = do
            s <- atomically $ readTVar gameState
            let (x, y) = (0, 0)
            (w, h) <- widgetGetSize canvas
            drawable <- widgetGetDrawWindow canvas
            drawWindowBeginPaintRect drawable (Rectangle 0 0 w h)
            renderWithDrawable drawable $ do
                setSourceSurface backgroundSurface (-x) (-y)
                paint
                mapM_ drawEntity [(e, x, y) | e <- stateEntities s, Just (x, y) <- [entityPosition e], not (entityOnTop e)]
                mapM_ drawEntity [(e, x, y) | e <- stateEntities s, Just (x, y) <- [entityPosition e], entityOnTop e]
            drawWindowEndPaint drawable
            return True
        drawEntity (e, x, y) = do
            save
            translate x y
            entityDraw e
            restore
        drawBackground m ps = do
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

gameLoop gameState keyState = forever $ atomically $ do
    s <- readTVar gameState
    k <- readTVar keyState
    let d = 0.5
    let us = map (\e -> entityUpdate e s d) ( stateEntities s )
    let s' = s { stateEntities = concat $ map updateEntities us }
    writeTVar gameState s'

