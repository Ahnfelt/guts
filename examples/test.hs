import Graphics.UI.Gtk  hiding (fill)
import Graphics.UI.Gtk.Gdk.Events
import Graphics.Rendering.Cairo
import Control.Concurrent
main :: IO ()
main = do
     initGUI
     window <- windowNew
     windowFullscreen window
     set window [windowTitle := "Guts",
                 windowDefaultWidth := 800, 
                 windowDefaultHeight := 600,
                 containerBorderWidth := 0]
                 
     canvas <- drawingAreaNew
     containerAdd window canvas

     widgetShowAll window 
     onExpose canvas (\x ->  do (w,h) <- widgetGetSize canvas
                                drawin <- widgetGetDrawWindow canvas
                                renderWithDrawable drawin 
                                    (myDraw (fromIntegral w)(fromIntegral h))
                                return (eventSent x))
    
     onDestroy window mainQuit
     mainGUI

myDraw :: Double -> Double -> Render ()
myDraw w h = do
           setSourceRGB 1 1 1
           paint

           setSourceRGB 0 0 0
           moveTo 0 0
           lineTo w h
           moveTo w 0
           lineTo 0 h
           setLineWidth (0.1 * (h + w))
           stroke

           rectangle 0 0 (0.5 * w) (0.5 * h)
           setSourceRGBA 1 0 0 0.8
           fill

           rectangle 0 (0.5 * h) (0.5 * w) (0.5 * h)
           setSourceRGBA 0 1 0 0.6
           fill

           rectangle (0.5 * w) 0 (0.5 * w) (0.5 * h)
           setSourceRGBA 0 0 1 0.4
           fill

