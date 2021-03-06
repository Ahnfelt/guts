module Main where

import Graphics.UI.Gtk  hiding (fill)
import Graphics.Rendering.Cairo
import Random
import Control.Monad
import Data.Maybe

import Mechanics
import Collision

main :: IO () 
--main = do testPointLine1; testPointLine2; testPointLine3; testPointLine4
--main = do testOrtogonal1; testOrtogonal2; testOrtogonal3; testOrtogonal4; testOrtogonal5; testOrtogonal6; testOrtogonal7
main = do testHole1a; testHole1b; testHole1c; testHole1d; testHole2
--main = forever testRandom

test1 :: IO ()
test1 = do
  let a = (0.2, 0.5)
  let b = (0.6, 0.2)
  let c = (0.4, 0.9)
  let d = (0.7, 0.3)
  let r = 0.25
  window a b c d r

testHole1a :: IO ()
testHole1a = do
    let s = 0.001
    let a = (200.0,0.0) .* s
    let b = (350.0,400.0) .* s
    let c = (260,238.5) .* s
    let d = (335,238.5) .* s
    let r = 10 * s
    window a b c d r

testHole1b :: IO ()
testHole1b = do
    let s = 0.001
    let b = (200.0,0.0) .* s
    let a = (350.0,400.0) .* s
    let c = (260,238.5) .* s
    let d = (335,238.5) .* s
    let r = 10 * s
    window a b c d r

testHole1c :: IO ()
testHole1c = do
    let s = 0.001
    let a = (200.0,0.0) .* s
    let b = (350.0,400.0) .* s
    let d = (260,238.5) .* s
    let c = (335,238.5) .* s
    let r = 10 * s
    window a b c d r

testHole1d :: IO ()
testHole1d = do
    let s = 0.001
    let b = (200.0,0.0) .* s
    let a = (350.0,400.0) .* s
    let d = (260,238.5) .* s
    let c = (335,238.5) .* s
    let r = 10 * s
    window a b c d r

testHole2 :: IO ()
testHole2 = do
    let s = 0.001
    let p = (0,50)
    let a = (200.0,0.0) .* s
    let b = (350.0,400.0) .* s
    let c = ((275.123164681647,238.53799999999998) .+ p) .* s
    let d = ((335.755324681647,238.53799999999998) .+ p) .* s
    let r = 10 * s
    window a b c d r


testOrtogonal1 :: IO ()
testOrtogonal1 = do
  let a = (0.3, 0.4)
  let b = (0.6, 0.4)
  let c = (0.5, 0.9)
  let d = (0.5, 0.1)
  let r = 0.1
  window a b c d r

testOrtogonal2 :: IO ()
testOrtogonal2 = do
  let a = (0.3, 0.4)
  let b = (0.6, 0.4)
  let d = (0.5, 0.9)
  let c = (0.5, 0.1)
  let r = 0.1
  window a b c d r

testOrtogonal3 :: IO ()
testOrtogonal3 = do
  let a = (0.5, 0.9)
  let b = (0.5, 0.1)
  let c = (0.2, 0.4)
  let d = (0.8, 0.4)
  let r = 0.1
  window a b c d r

testOrtogonal4 :: IO ()
testOrtogonal4 = do
  let a = (0.5, 0.9)
  let b = (0.5, 0.1)
  let d = (0.2, 0.4)
  let c = (0.8, 0.4)
  let r = 0.1
  window a b c d r

testOrtogonal5 :: IO ()
testOrtogonal5 = do
  let a = (0.5, 0.9)
  let b = (0.5, 0.1)
  let d = (0.2, 0.3)
  let c = (0.8, 0.4)
  let r = 0.1
  window a b c d r

testOrtogonal6 :: IO ()
testOrtogonal6 = do
  let a = (0.5, 0.9)
  let b = (0.6, 0.1)
  let d = (0.2, 0.4)
  let c = (0.8, 0.4)
  let r = 0.1
  window a b c d r

testOrtogonal7 :: IO ()
testOrtogonal7 = do
  let a = (0.5, 0.9)
  let b = (0.6, 0.1)
  let d = (0.2, 0.3)
  let c = (0.8, 0.4)
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
  
testPointLine1 :: IO ()
testPointLine1 = do
  let a = (0.2, 0.5)
  let b = (0.6, 0.2)
  let c = (0.2, 0.9)
  let d = (0.5, 0.3)
  let r = 0.25
  window a b c d r

testPointLine2 :: IO ()
testPointLine2 = do
  let b = (0.2, 0.5)
  let a = (0.6, 0.2)
  let c = (0.2, 0.9)
  let d = (0.5, 0.3)
  let r = 0.25
  window a b c d r

testPointLine3 :: IO ()
testPointLine3 = do
  let b = (0.3, 0.9)
  let a = (0.5, 0.4)
  let d = (0.2, 0.7)
  let c = (0.5, 0.1)
  let r = 0.25
  window a b c d r

testPointLine4 :: IO ()
testPointLine4 = do
  let b = (0.5, 0.5)
  let a = (0.8, 0.6)
  let d = (0.2, 0.7)
  let c = (0.5, 0.1)
  let r = 0.25
  window a b c d r


testRandom :: IO ()
testRandom = forever $ do
  (a, b, c, d, r) <- liftM5 (,,,,) v v v v (getStdRandom (randomR (0.05, 0.2)))
  window a b c d r
  where
    s = getStdRandom (randomR (0.1,0.9))
    v = liftM2 (,) s s
  

window ::  Vector -> Vector -> Vector -> Vector -> Double -> IO ()
window a b s v r = do
     initGUI
     window <- windowNew
     set window [windowTitle := "Moving circle VS Static line segment",
                 windowDefaultWidth := 800, windowDefaultHeight := 800,
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
                                return True)
    
     onDestroy window mainQuit
     mainGUI


drawCollision :: Double -> Vector -> Vector -> Vector -> 
                 Vector -> Double -> Render ()
drawCollision w a b c d r = do
    let u = b .- a
    let v = d .- c
    let Just i = intersection a u d v

    -- Background
    setSourceRGB 0.8 0.8 0.8
    paint

    -- Init Circle
    setSourceRGBA 1 1 0.7 0.3
    circle c r

    -- Static Line segment
    setSourceRGB 0.7 0.8 0.7
    line (a .- u .* 5) (b .+ u .* 5)
    setSourceRGB 0 0.6 0
    line a b

    -- Move indication line
    setSourceRGB 0.7 0.7 0.8
    line (c .- v .* 5) (d .+ v .* 5)
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

    setSourceRGBA 0 1 1 0.8
    case pointCircleCollision a c v r of
        Just (_, p) -> crossFixed p 8
        Nothing -> return ()

    case pointCircleCollision b c v r of
        Just (_, p) -> crossFixed p 8
        Nothing -> return ()

    where
      mod f = uncurry f . (.* w)
      circleFixed :: Vector -> Double -> Render ()
      circleFixed p r = do
        mod arc p r 0 (2 * pi)
        fill
      circle p r = circleFixed p (r*w)
      crossFixed :: Vector -> Double -> Render ()
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

