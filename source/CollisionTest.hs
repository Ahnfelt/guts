{-# LANGUAGE FlexibleInstances, UndecidableInstances, OverlappingInstances, 
  MultiParamTypeClasses, FunctionalDependencies, TypeSynonymInstances #-}
module Main where

import Graphics.UI.Gtk  hiding (fill)
import Graphics.Rendering.Cairo
import Random
import Control.Monad

type Vector a = (a, a)
type VectorD = Vector Double


main :: IO () 
main = testRandom
--main = forever testRandom

test1 :: IO ()
test1 = do
  let a = (0.2, 0.5)
  let b = (0.6, 0.2)
  let c = (0.4, 0.9)
  let d = (0.7, 0.3)
  let r = 0.25
  window a b c d r

test2 :: IO ()
test2 = do
  let a = (0.3, 0.8)
  let b = (0.6, 0.8)
  let c = (0.5, 0.9)
  let d = (0.5, 0.1)
  let r = 0.1
  window a b c d r

test3 :: IO ()
test3 = do
  let a = (0.2, 0.2)
  let b = (0.4, 0.2)
  let c = (0.4, 0.4)
  let d = (0.6, 0.2)
  let r = 0.15
  window a b c d r

testRandom :: IO ()
testRandom = forever $ do
  (a, b, c, d, r) <- liftM5 (,,,,) v v v v (getStdRandom (randomR (0.05, 0.2)))
  window a b c d r
  where
    s = getStdRandom (randomR (0,1))
    v = liftM2 (,) s s
  
-- Intersection between two parametirc lines. 
intersection :: VectorD -> VectorD -> VectorD -> VectorD -> VectorD
intersection (px, py) (vx, vy) q@(qx, qy) u@(ux, uy) =
    let z = vy / (uy*vx)
        t2 = ((py - qy)/uy + (qx - px)*z) / (1 - z*ux)
    in q .+. (u *. t2)

segmentCircleCollision :: VectorD -> VectorD -> VectorD -> VectorD -> Double -> Maybe (VectorD, VectorD)
segmentCircleCollision a b c d r =
  let v = d .-. c
      u = b .-. a
      i = intersection a u d v
      phi = angle u v 
      delta = r / tan phi
      i1 = i .-. norm u *. delta
      h = r / sin phi
      i2 = i .-. norm v *. h
  in if pointBoxCollision i2 (c, d) then Just (i1, i2) else Nothing
  
pointBoxCollision :: VectorD -> (VectorD, VectorD) -> Bool
pointBoxCollision (x, y) ((x1, y1), (x2, y2)) =
    ((x >= x1 && x <= x2) || (x <= x1 && x >= x2)) && 
    ((y >= y1 && y <= y2) || (y <= y1 && y >= y2))

drawCollision :: Double -> VectorD -> VectorD -> VectorD -> 
                 VectorD -> Double -> Render ()
drawCollision w a b c d r = do
  let v = d .-. c
  let u = b .-. a
  let i = intersection a u d v

  -- Background
  setSourceRGB 0.8 0.8 0.8
  paint

  -- Init Circle
  setSourceRGBA 1 1 0.7 0.3
  circle c r

  -- Line segment
  setSourceRGB 0.7 0.8 0.7
  line (a .-. u *. 5) (b .+. u *. 5)
  setSourceRGB 0 0.6 0
  line a b

  -- Move indication line
  setSourceRGB 0.7 0.7 0.8
  line (c .-. v *. 5) (d .+. v *. 5)
  setSourceRGB 0 0 0.6
  line c d
  setSourceRGB 0.2 0.2 0.2
  crossFixed c 8
  setSourceRGB 0.2 0.2 0.2
  circleFixed d 3

  -- Extended line intersection
  setSourceRGB 1 1 0
  crossFixed i 8

  case segmentCircleCollision a b c d r of
    Just (i1, i2) -> do 
      -- Circle
      setSourceRGBA 1 0.6 0.6 0.5
      circle i2 r
      -- Entity collision point (Circle center)
      setSourceRGB 0.8 0 0
      crossFixed i2 8
      -- Line segment collision point
      setSourceRGB 0.2 1 0.2
      crossFixed i1 8
    Nothing -> return ()

    where
      mod f = uncurry f . (*. w)
      circleFixed :: VectorD -> Double -> Render ()
      circleFixed p r = do
        mod arc p r 0 (2 * pi)
        fill
      circle p r = circleFixed p (r*w)
      crossFixed :: VectorD -> Double -> Render ()
      crossFixed p s = do
        mod moveTo p 
        uncurry relMoveTo (-s/2, -s/2)
        uncurry relLineTo (s,s)
        mod moveTo p 
        uncurry relMoveTo (s/2, -s/2)
        uncurry relLineTo (-s,s)
        stroke
      line a b = do
        mod moveTo a
        mod lineTo b
        setLineWidth 2
        stroke


window ::  VectorD -> VectorD -> VectorD -> VectorD -> Double -> IO ()
window a b s v r = do
     initGUI
     window <- windowNew
     set window [windowTitle := "Moving circle VS Static line segment",
                 windowDefaultWidth := 400, windowDefaultHeight := 400,
                 containerBorderWidth := 0 ]

     frame <- frameNew
     containerAdd window frame
     canvas <- drawingAreaNew
     containerAdd frame canvas

     widgetShowAll window 
     onExpose canvas (\x ->  do (w,h) <- widgetGetSize canvas
                                drawin <- widgetGetDrawWindow canvas
                                renderWithDrawable drawin 
                                    (drawCollision (fromIntegral w) a b s v r)
                                return (eventSent x))
    
     onDestroy window mainQuit
     mainGUI


class (Eq a, Show a) => Add a  where
    (.+.) :: a -> a -> a
    (.-.) :: a -> a -> a
                                    
instance (Add a, Add b) => Add (a, b) where
    (a, b) .+. (c, d) = (a .+. c, b .+. d)
    (a, b) .-. (c, d) = (a .-. c, b .-. d)

instance (Num a) => Add a where
    (.+.)  = (Prelude.+)
    (.-.)  = (Prelude.-)


(*.) :: (Num a) => Vector a -> a -> Vector a
(a, b) *. d = (a*d, b*d)

(/.) :: (Fractional a) => Vector a -> a -> Vector a
(a, b) /. d = (a/d, b/d)

-- right level ??
infixl 5 .+.
infixl 5 .-.
infixr 6 *.
infixr 6 /.

vectorLength :: (Floating a) => Vector a -> a
vectorLength (a,b) = sqrt (a*a + b*b)

norm :: (Floating a) => Vector a -> Vector a
norm v = v /. vectorLength v

dot :: (Num a) => Vector a -> Vector a -> a
dot (a, b) (c, d) = a*c + b*d

angle :: (Floating a) => Vector a -> Vector a -> a
angle v1 v2 = acos (norm v1 `dot` norm v2)


--class Mul a b c | a b -> c where
--  (.*.) :: a -> b -> c

--instance Num a => Mul (a, a) a (a, a) where
--    (a, b) .*. d = (a*d, b*d)


-- class (Eq a, Show a) => Add a  where
--     (+) :: a -> a -> a
--     (-) :: a -> a -> a
                                    
-- instance (Add a, Add b) => Add (a, b) where
--     (a, b) + (c, d) = (a Collision.+ c, b Collision.+ d)
--     (a, b) - (c, d) = (a Collision.- c, b Collision.- d)

-- instance Add Double where
--     (+)  = (Prelude.+)
--     (-)  = (Prelude.-)

--instance (Num a) => Add a where
--    a + b  = a Prelude.+ b
--    a - b  = a Prelude.- b
