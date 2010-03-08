module BasePainter (blockPainter) where
import Graphics.Rendering.Cairo
import System.Random
import Control.Monad (when)
import Tile

blockPainter :: Int -> IO TilePainter
blockPainter s = do
    return $ \t ts1 ts2 x y s -> case t of
        (TileBase BaseBlock) -> paintBlock t ts1 ts2 x y s
        _ -> return ()

paintBlock t (tn, ts, tw, te) ts2 x y s = do
    drawShadow x y tileWidth tileHeight (like tn) (like tw) 8 10
    setLineWidth 1.5
    setSourceRGB 0.28 0.28 0.28
    rectangle (fromIntegral x + 1) (fromIntegral y + 1) (fromIntegral tileWidth - 1 * 2) (fromIntegral tileHeight - 1 * 2)
    stroke
    setSourceRGB 0.32 0.32 0.32
    rectangle (fromIntegral x + 2.5) (fromIntegral y + 2.5) (fromIntegral tileWidth - 2.5 * 2) (fromIntegral tileHeight - 2.5 * 2)
    stroke
    setSourceRGB 0.40 0.40 0.40
    rectangle (fromIntegral x + 4) (fromIntegral y + 4) (fromIntegral tileWidth - 4 * 2) (fromIntegral tileHeight - 4 * 2)
    stroke
    setSourceRGB 0.35 0.35 0.35
    rectangle (fromIntegral x + 5.5) (fromIntegral y + 5.5) (fromIntegral tileWidth - 5.5 * 2) (fromIntegral tileHeight - 5.5 * 2)
    fill
    paintCracks (fromIntegral x) (fromIntegral y) (fromIntegral tileWidth) (fromIntegral tileHeight) s
    where
        like t = tileLike BaseBlock t

paintCracks x y w h s = do
    let r1:r2:r3:r4:r5:r6:_ = randoms $ mkStdGen s
    let x1s = randomRs (x, x + w - 1) $ mkStdGen r1
    let y1s = randomRs (y, y + h - 1) $ mkStdGen r2
    let x2s = randomRs (-5, 5) $ mkStdGen r3
    let y2s = randomRs (-5, 5) $ mkStdGen r4
    let x3s = randomRs (-5, 5) $ mkStdGen r5
    let y3s = randomRs (-5, 5) $ mkStdGen r6
    mapM_ drawCrack (take 30 $ zip3 (zip x1s y1s) (zip x2s y2s) (zip x3s y3s))
    where
        drawCrack ((x1, y1), (x2, y2), (x3, y3)) = do
            setSourceRGBA 0.20 0.20 0.20 0.05
            setLineWidth 2.0
            moveTo x1 y1
            lineTo (x1 + x2) (y1 + y2)
            lineTo (x1 + x2 + x3) (y1 + y2 + y3)
            stroke
            setSourceRGBA 0.20 0.20 0.20 0.15
            setLineWidth 1.0
            moveTo x1 y1
            lineTo (x1 + x2) (y1 + y2)
            lineTo (x1 + x2 + x3) (y1 + y2 + y3)
            stroke

drawShadow x y w h tn tw xw yw = do
    setSourceRGBA 0.0 0.0 0.0 0.25
    moveTo (fromIntegral x) (fromIntegral y)
    lineTo (fromIntegral x + fromIntegral tileWidth) (fromIntegral y + if tn then yw else 0)
    lineTo (fromIntegral x + fromIntegral tileWidth + xw) (fromIntegral y + yw)
    lineTo (fromIntegral x + fromIntegral tileWidth + xw) (fromIntegral y + fromIntegral tileHeight + yw)
    lineTo (fromIntegral x + xw) (fromIntegral y + fromIntegral tileHeight + yw)
    lineTo (fromIntegral x + if tw then xw else 0) (fromIntegral y + fromIntegral tileHeight)
    lineTo (fromIntegral x) (fromIntegral y)
    fill

