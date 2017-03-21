import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

main :: IO ()
main = do
  -- Initialize OpenGL via GLUT
  (progname, _) <- getArgsAndInitialize

  -- Create the output window
  createWindow progname

-- Every time the window needs to be updated, call the display function
  displayCallback $= display

  -- Let GLUT handle the window events, calling the displayCallback as fast as it can
  mainLoop

circle (x, y) radius divs = map toPoint angles where 
    arc       = (pi/2) / fromIntegral divs
    toPoint a = (x + cos a * radius, y + sin a * radius)
    angles    = map ((*arc) . fromIntegral) [0..divs]

display :: IO ()
display = do
  -- Clear the screen with the default clear color (black)
  clear [ ColorBuffer ]

  let points = circle (0,0) (1) 500 :: [(GLfloat, GLfloat)]

  renderPrimitive LineStrip $ do
    mapM_ (\(x, y) -> vertex $ Vertex2 x y) points
    

  -- Send all of the drawing commands to the OpenGL server
  flush

