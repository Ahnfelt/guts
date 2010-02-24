module Collision where

import Data.Maybe
import Data.List
import Data.Ord

import Mechanics

  
-- ab is the static line segment and cd is the movement. r is radius.
segmentCircleCollision :: Vector -> Vector -> Vector -> Vector -> Double -> Maybe (Vector, Vector)
segmentCircleCollision a b c d r =
  let u = b .- a
      v = d .- c
      (i1, i2) = lineCircleCollision a u c v r
      p1 = pointCircleCollision a c v r
      p2 = pointCircleCollision b c v r
      ps = filter (flip pointBoxCollision (c, d) . snd) $ catMaybes [p1, p2]
  in --Just (i1, i2) 
    if pointBoxCollision i2 (c, d) && pointBoxCollision i1 (a, b) then Just (i1, i2) 
    else if ps /= [] then Just $ snd $ minimumBy (comparing fst) $ zip (map (squaredDistance c . snd) ps) ps 
    else Nothing
{-    
    if pointBoxCollision i2 (c, d) then
    if pointBoxCollision i1 (a, b) then Just (i1, i2) 
    else if ps /= [] then Just $ snd $ minimumBy (comparing fst) $ zip (map (squaredDistance c . snd) ps) ps 
    else Nothing else Nothing
-}
    
-- (p + tu) is the static line and (q + tv) is the line on which the cicle moves. r is the radius. 
-- Returns (static line collision point, circle center)
-- TODO parallel lines!
lineCircleCollision :: Vector -> Vector -> Vector -> Vector -> Double -> (Vector, Vector)
lineCircleCollision p u q v r =
  let i = intersection p u q v
      phi = angle u v 
      delta = r / tan phi
      i1 = i .- norm u .* delta
      h = r / sin phi
      i2 = i .- norm v .* h
  in (i1, i2)

-- point (p+tv) radius
pointCircleCollision :: Vector -> Vector -> Vector -> Double -> Maybe (Vector, Vector)
pointCircleCollision (ax, ay) p@(px, py) v@(vx, vy) r = 
    let a = vx^2 + vy^2
        b = 2 * (vx * (px - ax) + vy * (py - ay))
        c = (px - ax)^2 + (py - ay)^2 - r^2
        d = b^2 - 4*a*c
        -- This solving method (based on the sign of b) is used to preserve 
        -- precision in the case where the value of b is close to that of sqrt d
        t = if b > 0 then (-b - sqrt d) / (2 * a) 
                     else let t1 = (-b + sqrt d) / (2 * a) 
                          in c / (a*t1)
        --t = (-b - sqrt d) / (2 * a)
    in if d >= 0 && a /= 0 then Just ((ax, ay), p .+ (v .* t)) else Nothing


pointBoxCollision :: Vector -> (Vector, Vector) -> Bool
pointBoxCollision (x, y) ((x1, y1), (x2, y2)) =
    ((x >= x1 && x <= x2) || (x <= x1 && x >= x2)) && 
    ((y >= y1 && y <= y2) || (y <= y1 && y >= y2))

-- Intersection between two parametirc lines. 
intersection :: Vector -> Vector -> Vector -> Vector -> Vector
intersection (px, py) (ux, uy) q@(qx, qy) v@(vx, vy) | vy*ux /= 0 =
    let z = uy / (vy*ux)
        t2 = ((py - qy)/vy + (qx - px)*z) / (1 - z*vx)
    in q .+ (v .* t2)
intersection p u q v = intersection q v p u

squaredDistance :: Vector -> Vector -> Double
squaredDistance (ax, ay) (bx, by) = (ax - bx)^2 + (ay - by)^2

