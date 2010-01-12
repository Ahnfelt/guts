import Data.IORef
import System.IO.Unsafe
import Graphics.UI.Gtk hiding (fill, Solid)
import Graphics.UI.Gtk.Gdk.Events
import Graphics.Rendering.Cairo
import Control.Concurrent
import Control.Monad (when)
import Control.Monad.Trans
import Data.Array.Diff

data Tile = Solid | Nonsolid deriving Enum

tileMap :: DiffArray (Int, Int) Tile
tileMap = listArray ((0, 0), (99, 99)) (concat $ repeat [Solid, Nonsolid, Solid])

tileWidth = 32
tileHeight = 32

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

    withImageSurfaceFromPNG "blah.png" $ \surface -> do

        canvas <- drawingAreaNew
        containerAdd window canvas

        widgetShowAll window 

        onKeyPress window $ \Key { eventKeyName = key } ->
            when (key == "Escape") mainQuit >> return True

        color <- newIORef (1, 0, 0)
        timeoutAdd (updateGraphics canvas color surface) 100
        onDestroy window mainQuit
        mainGUI
    
    where
        updateGraphics canvas color surface = do
            (r, g, b) <- readIORef color
            (w, h) <- widgetGetSize canvas
            drawable <- widgetGetDrawWindow canvas
            drawWindowBeginPaintRect drawable (Rectangle 0 0 w h)
            --renderWithDrawable drawable (myDraw (fromIntegral w) (fromIntegral h) (r, g, b) surface)
            renderWithDrawable drawable (drawTiles (w `div` 2) (h `div` 2) 10 10 tileMap surface)
            drawWindowEndPaint drawable
            writeIORef color (b, r, g)
            return True

drawTiles :: Int -> Int -> Int -> Int -> DiffArray (Int, Int) Tile -> Surface -> Render ()
drawTiles w h x y tileMap surface = do
    let (oX, oY) = (x `mod` tileWidth, y `mod` tileHeight)
    let (nwX, nwY) = (x `div` tileWidth, y `div` tileHeight)
    let (seX, seY) = ((x + w) `div` tileWidth, (y + h) `div` tileHeight)
    let coordinates = range ((nwX, nwY), (seX, seY))
    scale 2 2
    mapM_ (draw oX oY) coordinates
    where
        draw oX oY (tX, tY) = do
            case tileMap ! (tX, tY) of
                Solid -> do
                    let x = fromIntegral (tX * tileWidth - oX)
                    let y = fromIntegral (tY * tileHeight - oY)
                    setSourceSurface surface x y
                    paint
                Nonsolid -> do
                    return ()

