module Barrier where
import Data.Array.Diff
import Mechanics
import Tile

newtype BarrierMap = BarrierMap (DiffArray (Int, Int) [LineSegment])

gridWidth = tileWidth
gridHeight = tileHeight

newBarrierMap :: Int -> Int -> BarrierMap
newBarrierMap w h = 
    let (w', h') = (w `div` gridWidth, h `div` gridHeight) in 
    BarrierMap (listArray ((0, 0), (w' - 1, h' - 1)) (repeat []))

addBarrier :: BarrierMap -> LineSegment -> BarrierMap
addBarrier (BarrierMap a) l =
    let cs = cellsNear l 0 in
    BarrierMap (accum (++) a (zip cs (repeat [l])))

barriersNear :: BarrierMap -> LineSegment -> Double -> [LineSegment]
barriersNear (BarrierMap a) l r = concatMap (a!) (cellsNear l r)

cellsNear :: LineSegment -> Double -> [(Int, Int)]
cellsNear ((x1, y1), (x2, y2)) r | x1 > x2 = cellsNear ((x2, y1), (x1, y2)) r
cellsNear ((x1, y1), (x2, y2)) r | y1 > y2 = cellsNear ((x1, y2), (x2, y1)) r
cellsNear ((x1, y1), (x2, y2)) r =
    let (x1', y1') = gridCoordinate (x1 - r, y1 - r) in
    let (x2', y2') = gridCoordinate (x2 + r, y2 + r) in
    [(x, y) | x <- [x1' .. x2'], y <- [y1' .. y2'], x >= 0, x < gridWidth, y >= 0, y < gridHeight]
    
gridCoordinate :: Vector -> (Int, Int)
gridCoordinate (x, y) = (truncate x `div` gridWidth, truncate y `div` gridHeight)

