-- This module is a starting point for implementing the Graph Drawing
-- Calculator as described in Part II of the Standard Lab. You can use this
-- directly, or just study it as an example of how to use threepenny-gui.
import Expr
import ThreepennyPages
import Graphics.UI.Threepenny.Core as UI
import qualified Graphics.UI.Threepenny as UI


canWidth,canHeight :: Num a => a
canWidth  = 300
canHeight = 300


main :: IO ()
main = startGUI defaultConfig setup

setup :: Window -> UI ()
setup window = do -- Create them user interface elements
     canvas  <- mkCanvas canWidth canHeight   -- The drawing area
     fx      <- mkHTML "<i>f</i>(<i>x</i>)="  -- The text "f(x)="
     input   <- mkInput 20 "x"                -- The formula input
     draw    <- mkButton "Draw graph"         -- The draw button
     zoomSlider <- mkSlider (1, 100) 20       -- Add a slider for zooming
     diffButton <- mkButton "Differentiate"   -- Create a Differentiate button
    
    
       -- The markup "<i>...</i>" means that the text inside should be rendered
       -- in italics.

     -- Add the user interface elements to the page, creating a specific layout
     formula <- row [pure fx,pure input]
     getBody window #+ [column [pure canvas,pure formula,pure draw, pure zoomSlider, pure diffButton]]

     -- Styling
     getBody window # set style [("backgroundColor","lightblue"),
                                 ("textAlign","center")]
     pure input # set style [("fontSize","14pt")]

     -- Interaction (install event handlers)
     on UI.click     draw  $ \ _ -> readAndDraw input canvas zoomSlider
     on valueChange' input $ \ _ -> readAndDraw input canvas zoomSlider
     on valueChange' zoomSlider $ \_ -> readAndDraw input canvas zoomSlider
     on UI.click diffButton $ \_ -> differentiateAndDraw input canvas zoomSlider

-- readAndDraw takes an input field, a canvas, and a zoom control element.
-- If the expression is valid, it draws a path representing the expression with the given zoom factor.
readAndDraw :: Element -> Canvas  -> Element -> UI ()
readAndDraw input canvas zoom = do
    formula <- get value input 
    zoomFactor <- get value zoom

    case readExpr formula of
      Just expr -> do
        clearCanvas canvas
        let scale = read zoomFactor  
            pointList = points expr scale (canWidth, canHeight)
        path "blue" pointList canvas

      Nothing   -> return ()


-- differentiateAndDraw takes an input field, a canvas, and a zoom control slider. It updates the
-- input field with the differentiated expression, and draws the new expression on
-- the canvas with the specified zoom factor.
differentiateAndDraw :: Element -> Canvas -> Element -> UI ()
differentiateAndDraw input canvas zoomSlider = do
    formula <- get value input
    zoomFactor <- get value zoomSlider

    case readExpr formula of
      Just expr -> do
        let diffExpr = differentiate expr
            scale = read zoomFactor
            pointList = points diffExpr scale (canWidth, canHeight)

        element input # set value (showExpr diffExpr)
        clearCanvas canvas
        path "blue" pointList canvas

      Nothing   -> return ()

-- | 'points' takes an expression, a scaling factor, and canvas dimensions (width, height).
-- It generates a list of points representing the expression evaluated at each x-coordinate
-- across the canvas, scaled and positioned according to the given dimensions and scale.
points :: Expr -> Double -> (Int, Int) -> [Point]
points exp scale (width, height) =
  [ (fromIntegral x, fromIntegral y)
    | x <- [0 .. width - 1]
    , let val = eval exp ((fromIntegral x - fromIntegral (width `div` 2)) / scale)
          y = height - round (val * scale + fromIntegral (height `div` 2))
  ]