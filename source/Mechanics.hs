module Mechanics where
    
type Vector = (Double, Double)
type Position = Vector
type Velocity = Vector
type Dimension = Vector
type Angle = Double
type Magnitude = Double
type Duration = Double

-- Velocity from angle and magnitude
velocity :: Angle -> Magnitude -> Velocity
velocity a m = (cos a * m, sin a * m)

-- Operators for 2d vectors
(.+) :: Vector -> Vector -> Vector
(.+) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
(.*) :: Vector -> Magnitude -> Vector
(.*) (x, y) m = (x * m, y * m)

-- Approximate an angle a' from a, taking 1/d iterations to turn a full circle
approximateAngle :: Duration -> Angle -> Angle -> Angle
approximateAngle t a a' = 
    let a'' = atan2 (sin (a' - a)) (cos (a' - a)) in
    if t > abs a'' then a'
    else a + signum a'' * t

-- Interpolates linearly between a series of values (elapsed, value, [(time, value)])
-- For easiest usage, let 0 <= t <= 1. Durations cannot be negative.
interpolate :: Duration -> Double -> [(Duration, Double)] -> Double
interpolate t i _ | t <= 0 = i
interpolate t i ((d, v):ps) | t > d = interpolate (t - d) v ps
interpolate t i ((d, v):ps) = let f = t / (d + 0.00001) in (1.0 - f) * i + f * v
interpolate t i [] = i

