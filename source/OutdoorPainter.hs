module OutdoorPainter (outdoorPainter) where
import Graphics.Rendering.Cairo
import System.Random
import Control.Monad (when)
import Tile

outdoorPainter :: Int -> IO TilePainter
outdoorPainter s = do
    sparseGrassTiles <- mapM sparseGrassTile (take 30 $ randoms $ mkStdGen s)
    denseGrassTiles <- mapM denseGrassTile (take 30 $ randoms $ mkStdGen s)
    return $ \t ts1 ts2 x y s -> case t of
        OutdoorGrass -> Just (grassPainter sparseGrassTiles denseGrassTiles t ts1 ts2 x y s)
        _ -> Nothing
    where
        sparseGrassTile s = do
            let (w, h) = (fromIntegral tileWidth, fromIntegral tileHeight)
            let (m, d) = (5, 0.02)
            i <- createImageSurface FormatARGB32 h w
            renderWith i (drawGrass 0 0 (fromIntegral h) (fromIntegral w) m d s)
            return i
        denseGrassTile s = do
            let (w, h) = (tileWidth * 2, tileHeight * 2)
            let (m, d) = (5, 0.15)
            i <- createImageSurface FormatARGB32 (fromIntegral h) (fromIntegral w)
            renderWith i (drawGrass 0 0 (fromIntegral h) (fromIntegral w) m d s)
            return i

grassPainter sis dis _ (tn, ts, tw, te) (tnw, tne, tsw, tse) x y s = do
    let si1:si2:si3:si4:si5:_ = map (sis !!) $ randomRs (0, length sis - 1) (mkStdGen s)
    when (like tn && like ts && like tw && like te) $ do
        let di1:di2:_ = map (dis !!) $ randomRs (0, length sis - 1) (mkStdGen s)
        let (x', y') = (x - (fromIntegral $ tileWidth `div` 2), y - (fromIntegral $ tileHeight `div` 2))
        setSourceSurface di1 (fromIntegral x') (fromIntegral y')
        paint
    when (like tn) $ do
        let (x', y') = (fromIntegral x, fromIntegral y - fromIntegral tileHeight * 0.5)
        setSourceSurface si2 x' y'
        paint
    when (like tw) $ do
        let (x', y') = (fromIntegral x - fromIntegral tileWidth * 0.5, fromIntegral y)
        setSourceSurface si3 x' y'
        paint
    when (like tn && like tw && like tnw) $ do
        let (x', y') = (fromIntegral x - fromIntegral tileHeight * 0.5, fromIntegral y - fromIntegral tileHeight * 0.5)
        setSourceSurface si4 x' y'
        paint
        let (x', y') = (fromIntegral x - fromIntegral tileHeight * 0.5, fromIntegral y - fromIntegral tileHeight * 0.5)
        setSourceSurface si5 x' y'
        paint

        {-case (tn, ts, tw, te) of
        (OutdoorGrass, OutdoorGrass, OutdoorGrass, OutdoorGrass) -> do
        (OutdoorGrass, OutdoorGrass, _, _) -> do
            let (x', y') = (fromIntegral x, fromIntegral y - fromIntegral tileHeight * 0.5)
            setSourceSurface si2 x' y'
            paint
            let (x', y') = (fromIntegral x, fromIntegral y + fromIntegral tileHeight * 0.5)
            setSourceSurface si3 x' y'
            paint
        (_, _, OutdoorGrass, OutdoorGrass) -> do
            let (x', y') = (fromIntegral x - fromIntegral tileWidth * 0.5, fromIntegral y)
            setSourceSurface si2 x' y'
            paint
            let (x', y') = (fromIntegral x + fromIntegral tileWidth * 0.5, fromIntegral y)
            setSourceSurface si3 x' y'
            paint
        (OutdoorGrass, _, OutdoorGrass, _) -> do
            let (x', y') = (fromIntegral x, fromIntegral y - fromIntegral tileHeight * 0.5)
            setSourceSurface si2 x' y'
            paint
            let (x', y') = (fromIntegral x, fromIntegral y - fromIntegral tileHeight * 0.5)
            setSourceSurface si3 x' y'
            paint
            let (x', y') = (fromIntegral x - fromIntegral tileWidth * 0.5, fromIntegral y)
            setSourceSurface si2 x' y'
            paint
            let (x', y') = (fromIntegral x - fromIntegral tileWidth * 0.5, fromIntegral y)
            setSourceSurface si3 x' y'
            paint
        (OutdoorGrass, _, _, _) -> do
            let (x', y') = (fromIntegral x, fromIntegral y - fromIntegral tileHeight * 0.5)
            setSourceSurface si2 x' y'
            paint
        (_, _, OutdoorGrass, _) -> do
            let (x', y') = (fromIntegral x - fromIntegral tileWidth * 0.5, fromIntegral y)
            setSourceSurface si3 x' y'
            paint-}
    setSourceSurface si1 (fromIntegral x) (fromIntegral y)
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
    setLineWidth 3.0
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

