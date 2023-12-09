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

points :: Expr -> Double -> (Int, Int) -> [Point]
points exp scale (width, height) =
  [ (fromIntegral x, fromIntegral y)
    | x <- [0 .. width - 1]
    , let val = eval exp ((fromIntegral x - fromIntegral (width `div` 2)) / scale)
          y = height - round (val * scale + fromIntegral (height `div` 2))
  ]

main :: IO ()
main = startGUI defaultConfig setup

setup :: Window -> UI ()
setup window =
  do -- Create them user interface elements
     canvas  <- mkCanvas canWidth canHeight   -- The drawing area
     fx      <- mkHTML "<i>f</i>(<i>x</i>)="  -- The text "f(x)="
     input   <- mkInput 20 "x"                -- The formula input
     draw    <- mkButton "Draw graph"         -- The draw button
       -- The markup "<i>...</i>" means that the text inside should be rendered
       -- in italics.

     -- Add the user interface elements to the page, creating a specific layout
     formula <- row [pure fx,pure input]
     getBody window #+ [column [pure canvas,pure formula,pure draw]]

     -- Styling
     getBody window # set style [("backgroundColor","lightblue"),
                                 ("textAlign","center")]
     pure input # set style [("fontSize","14pt")]

     -- Interaction (install event handlers)
     on UI.click     draw  $ \ _ -> readAndDraw input canvas
     on valueChange' input $ \ _ -> readAndDraw input canvas


-- readAndDraw :: Element -> Canvas -> UI ()
-- readAndDraw input canvas =
--   do -- Get the current formula (a String) from the input element
--      formula <- get value input
--      -- Clear the canvas
--      clearCanvas canvas
--      -- The following code draws the formula text in the canvas and a blue line.
--      -- It should be replaced with code that draws the graph of the function.
--      set UI.fillStyle (UI.solidColor (UI.RGB 0 0 0)) (pure canvas)
--      UI.fillText formula (10,canHeight/2) canvas
--      path "blue" [(10,10),(canWidth-10,canHeight/2)] canvas

-- Function to read and draw expression graph
readAndDraw :: Element -> Canvas -> UI ()
readAndDraw input canvas = do
    formula <- get value input
    clearCanvas canvas
    case readExpr formula of
        Just expr -> drawGraph canvas expr
        Nothing   -> return ()  -- or handle error

-- Function to draw the graph of an expression
drawGraph :: Canvas -> Expr -> UI ()
drawGraph canvas expr = do
    let scale = 20  -- Adjust scale as necessary
    let graphPoints = points expr scale (canWidth, canHeight)
    path "red" graphPoints canvas


