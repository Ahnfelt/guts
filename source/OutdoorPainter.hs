module OutdoorPainter (roadPainter, grassPainter) where
import Graphics.Rendering.Cairo
import System.Random
import Control.Monad (when)
import Tile

grassPainter :: Int -> IO TilePainter
grassPainter s = do
    grassTiles <- mapM grassTile (take 30 $ randoms $ mkStdGen s)
    return $ \t ts1 ts2 x y s -> case t of
        OutdoorGrass -> Just (paintGrass grassTiles t ts1 ts2 x y s)
        _ -> Nothing
    where
        grassTile s = do
            let (w, h) = (fromIntegral tileWidth, fromIntegral tileHeight)
            let (m, d) = (5, 0.02)
            i <- createImageSurface FormatARGB32 h w
            renderWith i (drawGrass 0 0 (fromIntegral h) (fromIntegral w) m d s)
            return i

paintGrass sis _ (tn, ts, tw, te) (tnw, tne, tsw, tse) x y s = do
    setSourceRGB 0.10 0.25 0.05
    rectangle (fromIntegral x) (fromIntegral y) (fromIntegral tileWidth) (fromIntegral tileHeight)
    fill
    let si1:si2:si3:si4:si5:si6:si7:si8:si9:si10:si11:si12:si13:_ = map (sis !!) $ randomRs (0, length sis - 1) (mkStdGen s)
    let (x', y') = (fromIntegral x, fromIntegral y - fromIntegral tileHeight * 0.5)
    setSourceSurface si1 x' y'
    paint
    let (x', y') = (fromIntegral x, fromIntegral y + fromIntegral tileHeight * 0.5)
    setSourceSurface si2 x' y'
    paint
    let (x', y') = (fromIntegral x - fromIntegral tileWidth * 0.5, fromIntegral y)
    setSourceSurface si3 x' y'
    paint
    let (x', y') = (fromIntegral x + fromIntegral tileWidth * 0.5, fromIntegral y)
    setSourceSurface si4 x' y'
    paint
    when (like tn) $ do
        let (x', y') = (fromIntegral x - fromIntegral tileWidth * 0.5, fromIntegral y - fromIntegral tileHeight * 0.5)
        setSourceSurface si5 x' y'
        paint
        let (x', y') = (fromIntegral x + fromIntegral tileWidth * 0.5, fromIntegral y - fromIntegral tileHeight * 0.5)
        setSourceSurface si6 x' y'
        paint
    when (like tw) $ do
        let (x', y') = (fromIntegral x - fromIntegral tileWidth * 0.5, fromIntegral y - fromIntegral tileHeight * 0.5)
        setSourceSurface si7 x' y'
        paint
        let (x', y') = (fromIntegral x - fromIntegral tileWidth * 0.5, fromIntegral y + fromIntegral tileHeight * 0.5)
        setSourceSurface si8 x' y'
        paint
    let (x', y') = (fromIntegral x - fromIntegral tileWidth * 0.3, fromIntegral y - fromIntegral tileHeight * 0.3)
    setSourceSurface si9 x' y'
    paint
    let (x', y') = (fromIntegral x + fromIntegral tileWidth * 0.3, fromIntegral y - fromIntegral tileHeight * 0.3)
    setSourceSurface si10 x' y'
    paint
    let (x', y') = (fromIntegral x - fromIntegral tileWidth * 0.3, fromIntegral y + fromIntegral tileHeight * 0.3)
    setSourceSurface si11 x' y'
    paint
    let (x', y') = (fromIntegral x + fromIntegral tileWidth * 0.3, fromIntegral y + fromIntegral tileHeight * 0.3)
    setSourceSurface si12 x' y'
    paint
    setSourceSurface si13 (fromIntegral x) (fromIntegral y)
    paint
    where
        like OutdoorGrass = True
        like _ = False

drawGrass x y w h m d s = do
    let r1:r2:r3:rs = map mkStdGen $ randoms (mkStdGen s)
    let xs = randomRs (x + m, x + w - m) r1
    let ys = randomRs (y + m, y + h - m) r2
    let l = take (round (w * h * d)) $ zip3 rs xs ys
    mapM_ drawStraw l

drawStraw (r, x, y) = do
    setLineWidth 4.0
    setLineCap LineCapRound
    setSourceRGB 0.1 0.2 0
    let (b, r') = randomR (0, 2 * pi) r
    let (e, _) = randomR (0, 0.1 * pi + 0.5 * pi) r'
    arc (x) (y) 5 b (b + e)
    stroke
    setLineWidth 2.0
    setSourceRGB 0.1 0.5 0
    arc (x) (y) 5 b (b + e)
    stroke


roadPainter :: Int -> IO TilePainter
roadPainter s = do
    return $ \t ts1 ts2 x y s -> case t of
        OutdoorRoad -> Just (paintRoad t ts1 ts2 x y s)
        _ -> Nothing

paintRoad t ts1 ts2 x y s = do
    setSourceRGB 0.20 0.20 0.10
    rectangle (fromIntegral x) (fromIntegral y) (fromIntegral tileWidth) (fromIntegral tileHeight)
    fill
    let r1:r2:r3:r4:r5:r6:rs = map mkStdGen $ randoms (mkStdGen s)
    let xs = randomRs (fromIntegral x, fromIntegral x + fromIntegral tileWidth) r1
    let ys = randomRs (fromIntegral y, fromIntegral y + fromIntegral tileHeight) r2
    let zs = randomRs (1, 5) r3
    let cs = randomRs (0.10, 0.20) r4
    let as = randomRs (0, 2 * pi) r5
    let bs = randomRs (pi / 2, pi) r6
    mapM_ drawCrack $ take 100 (zip3 (zip xs ys) (zip zs cs) (zip as bs))
    mapM_ drawStones $ take 100 (zip3 (zip xs ys) (zip zs cs) (zip as bs))
    where
        drawCrack ((x, y), (z, cs), (a, b)) = do
            setSourceRGB (cs - 0.1) (cs - 0.1) cs
            arc x y (z + 1) a (a + b)
            fill
            setSourceRGB cs cs cs
            arc x y z a (a + b)
            fill
        drawStones ((x, y), (z, cs), (a, b)) = do
            setSourceRGB cs cs cs
            arc x y (z / 2 + 1) 0 pi
            fill


