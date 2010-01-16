module OutdoorPainter (rockPainter, grassPainter, flowerPainter) where
import Graphics.Rendering.Cairo
import System.Random
import Control.Monad (when)
import Tile

grassPainter :: (Tile -> Bool) -> Double -> Int -> IO TilePainter
grassPainter f z s = do
    grassTiles <- mapM (grassTile z) (take 30 $ randoms $ mkStdGen s)
    return $ \t ts1 ts2 x y s -> if f t
        then paintGrass f z grassTiles t ts1 ts2 x y s
        else return ()
    where
        grassTile z s = do
            let (w, h) = (fromIntegral tileWidth, fromIntegral tileHeight)
            let (m, d) = (5, 0.03)
            i <- createImageSurface FormatARGB32 h w
            renderWith i (drawGrass z 0 0 (fromIntegral h) (fromIntegral w) m d s)
            return i

paintGrass like z sis _ (tn, ts, tw, te) (tnw, tne, tsw, tse) x y s = do
    setSourceRGB (0.05 * z) (0.20 * z) (0.05 * z)
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

drawGrass z x y w h m d s = do
    let r1:r2:r3:rs = map mkStdGen $ randoms (mkStdGen s)
    let xs = randomRs (x + m, x + w - m) r1
    let ys = randomRs (y + m, y + h - m) r2
    let l = take (round (w * h * d)) $ zip3 rs xs ys
    mapM_ (drawStraw z) l

drawStraw z (r, x, y) = do
    let (b, r') = randomR (0, 2 * pi) r
    let (e, r'') = randomR (0, 0.1 * pi + 0.5 * pi) r'
    let (l, r''') = randomR (2.0, 4.0) r''
    let (c, r'''') = randomR (0.3, 0.5) r'''
    let (s, _) = randomR (4.0, 6.0) r''''
    setLineWidth l
    setLineCap LineCapRound
    setSourceRGB (0.05 * z) (0.2 * z) 0.0
    arc (x) (y) s b (b + e)
    stroke
    setLineWidth (l / 2)
    setSourceRGB (0.1 * z) (c * z) 0.0
    arc (x) (y) s b (b + e)
    stroke


flowerPainter :: Int -> IO TilePainter
flowerPainter s = do
    return $ \t ts1 ts2 x y s -> case t of
        OutdoorBush -> paintFlower x y s
        _ -> return ()
    where
        paintFlower x y s = do
            let r1:r2:r3:r4:r5:_ = map mkStdGen $ randoms $ mkStdGen s
            when ((fst (random r1) :: Double) < 0.50) $ do
                let x' = fromIntegral $ fst $ randomR (x, x + tileWidth - 1) r2
                let y' = fromIntegral $ fst $ randomR (y, y + tileHeight - 1) r3
                let a = fst $ randomR (0, 2 * pi) r4
                let z = fst $ randomR (0.5, 1) r5
                save
                translate x' y'
                rotate a
                mapM_ (drawPetal z 4 3) [0, pi/3 .. 2 * pi]
                setSourceRGBA 0.0 0.0 0.0 0.2
                arc 0 0 (3 * z) 0 (2 * pi)
                stroke
                setSourceRGB (1.0 * z) (0.9 * (0.5 + z * 0.5)) 0.0
                arc 0 0 (3 * z) 0 (2 * pi)
                fill
                restore
            where
                drawPetal z r d a = do
                    save
                    scale z z
                    rotate a
                    setSourceRGB (0.7 * z) (0.7 * (0.8 + z * 0.2)) (0.7 * z)
                    arc d 0 r 0 (2 * pi)
                    fill
                    setSourceRGBA 0.0 0.0 0.0 0.2
                    setLineWidth 1
                    arc d 0 r 0 (2 * pi)
                    stroke
                    restore


rockPainter :: Int -> IO TilePainter
rockPainter s = do
    return $ \t ts1 ts2 x y s -> case t of
        OutdoorRock -> paintRock t ts1 ts2 x y s
        _ -> return ()

paintRock t ts1 ts2 x y s = do
    setSourceRGB 0.10 0.10 0.10
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
            setSourceRGB (cs - 0.10) (cs - 0.10) (cs - 0.05)
            arc x y (z + 1) a (a + b)
            fill
            setSourceRGB cs cs cs
            arc x y z a (a + b)
            fill
        drawStones ((x, y), (z, cs), (a, b)) = do
            setSourceRGB cs cs cs
            arc x y (z / 2 + 1) 0 pi
            fill


