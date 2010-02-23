module CollisionTest where

import Graphics.UI.Gtk  hiding (fill)
import Graphics.Rendering.Cairo
import Random
import Control.Monad
import Data.Maybe
import Data.List
import Data.Ord

import Mechanics
import Collision

main :: IO () 
main = testPointLine4
--main = forever testRandom

test1 :: IO ()
test1 = do
  let a = (0.2, 0.5)
  let b = (0.6, 0.2)
  let c = (0.4, 0.9)
  let d = (0.7, 0.3)
  let r = 0.25
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
    let u = b .-. a
    let v = d .-. c
    let i = intersection a u d v

    -- Background
    setSourceRGB 0.8 0.8 0.8
    paint

    -- Init Circle
    setSourceRGBA 1 1 0.7 0.3
    circle c r

    -- Static Line segment
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

    setSourceRGBA 0 1 1 0.8
    case pointCircleCollision a c v r of
        Just (_, p) -> crossFixed p 8
        Nothing -> return ()

    case pointCircleCollision b c v r of
        Just (_, p) -> crossFixed p 8
        Nothing -> return ()
    

    where
      mod f = uncurry f . (*. w)
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

