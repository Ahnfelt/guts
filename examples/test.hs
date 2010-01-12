import Control.Concurrent.MVar
import System.IO.Unsafe
import Graphics.UI.Gtk hiding (fill, Solid)
import Graphics.UI.Gtk.Gdk.Events
import Graphics.Rendering.Cairo
import Control.Concurrent
import Control.Monad (when)
import Control.Monad.Trans
import Data.Array.Diff
import Data.List (transpose)
import qualified Data.Set as Set

graphicalFps = 30

data Tile = Tile { 
    tileSolid :: Bool,
    tileBelow :: Surface,
    tileAbove :: Surface
    }

tileWidth = 32
tileHeight = 32

ascii = concat $ transpose [
    "************************************************************",
    "*   *       *         *                                    *",
    "*   *  ***  * *** *   *                                    *",
    "*      *    *   * *   *                                    *",
    "********    *** * *** *                                    *",
    "*           *       *                                      *",
    "*           *       *                                      *",
    "* *******************                                      *",
    "*        *                                                 *",
    "*        *                                                 *",
    "*****    *                                                 *",
    "*   *    *                                                 *",
    "*   ******                                                 *",
    "*                                                          *",
    "*                                                          *",
    "*                                                          *",
    "*                                                          *",
    "*                                                          *",
    "*                                                          *",
    "*                                                          *",
    "*                                                          *",
    "*                                                          *",
    "*                                                          *",
    "*                                                          *",
    "*                                                          *",
    "*                                                          *",
    "*                                                          *",
    "*                                                          *",
    "*                                                          *",
    "************************************************************"
    ]

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

    withImageSurfaceFromPNG "tiles/none.png" $ \noSurface -> withImageSurfaceFromPNG "tiles/man1.png" $ \manSurface ->
     withImageSurfaceFromPNG "tiles/organic.png" $ \organicSurface ->
      withImageSurfaceFromPNG "tiles/wall.png" $ \boxBelowSurface -> 
       withImageSurfaceFromPNG "tiles/walltop.png" $ \boxAboveSurface -> do
        let tile '*' = Tile { tileSolid = True, tileBelow = boxBelowSurface, tileAbove = boxAboveSurface }
            tile ' ' = Tile { tileSolid = False, tileBelow = organicSurface, tileAbove = noSurface }
        
        let tileMap :: DiffArray (Int, Int) Tile
            tileMap = listArray ((0, 0), (59, 29)) (map tile ascii)

        canvas <- drawingAreaNew
        containerAdd window canvas

        widgetShowAll window 

        keyState <- newMVar (Set.empty)

        onKeyPress window $ \Key { eventKeyName = key } -> case key of
            "Escape" -> do 
                mainQuit
                return True
            k -> do 
                modifyMVar_ keyState $ \s -> return $ Set.insert k s
                return True

        onKeyRelease window $ \Key { eventKeyName = key } -> case key of
            k -> do 
                modifyMVar_ keyState $ \s -> return $ Set.delete k s
                return True

        timeoutAdd (updateGraphics canvas tileMap manSurface) (1000 `div` graphicalFps)
        onDestroy window mainQuit
        mainGUI
    
    where
        updateGraphics canvas tileMap manSurface = do
            (w, h) <- widgetGetSize canvas
            drawable <- widgetGetDrawWindow canvas
            drawWindowBeginPaintRect drawable (Rectangle 0 0 w h)
            renderWithDrawable drawable $ do
                scale 2 2
                drawTiles (w `div` 2) (h `div` 2) 10 10 (surfaceBelow tileMap)
                drawCharacters (w `div` 2) (h `div` 2) 10 10 manSurface
                drawTiles (w `div` 2) (h `div` 2) 10 10 (surfaceAbove tileMap)
            drawWindowEndPaint drawable
            return True
        surfaceBelow tileMap (x, y) = tileBelow (tileMap ! (x, y))
        surfaceAbove tileMap (x, y) = tileAbove (tileMap ! (x, y + 1))

drawCharacters :: Int -> Int -> Int -> Int -> Surface -> Render ()
drawCharacters w h x y surface = do
    setSourceSurface surface 120 80
    paint

drawTiles :: Int -> Int -> Int -> Int -> ((Int, Int) -> Surface) -> Render ()
drawTiles w h x y surface = do
    let (oX, oY) = (x `mod` tileWidth, y `mod` tileHeight)
    let (nwX, nwY) = (x `div` tileWidth, y `div` tileHeight)
    let (seX, seY) = ((x + w) `div` tileWidth, (y + h) `div` tileHeight)
    let coordinates = range ((nwX, nwY), (seX, seY))
    mapM_ (draw oX oY) coordinates
    where
        draw oX oY (tX, tY) = do
            let s = surface (tX, tY)
            let x = fromIntegral (tX * tileWidth - oX)
            let y = fromIntegral (tY * tileHeight - oY)
            setSourceSurface s x y
            paint

