-- for this to work OpenGL and GLUT have to be in firsProject.cabal
-- and stack build has to be called 
-- (I did stack install OpenGL before)
-- for this to worok the appropriate C-Libraries have to be installed
-- sudo apt-get install libgl1-mesa-dev libglu1-mesa-dev freeglut3-dev
-- for the graphics I followed https://wiki.haskell.org/OpenGLTutorial1
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
myPoints :: [(GLfloat,GLfloat,GLfloat)]
myPoints = [(sin (2*pi*k/12),cos (2*pi*k/12),0)| k<-[1..12]]
main :: IO ()
main = do
  (_progName,args) <-getArgsAndInitialize
  _window <- createWindow "Hello World"
  displayCallback $= display
  mainLoop

display :: DisplayCallback
display = do
  clear [ ColorBuffer ]
  -- renderPrimitive Points $
  -- renderPrimitive Triangles $
  -- renderPrimitive TriangleStrip$
  renderPrimitive TriangleFan$
    mapM_ (\(x,y,z) -> vertex $ Vertex3 x y z) myPoints
  flush

-- rim diameter without horns 622mm (29" or 28" 700C)  
-- with horns the values differ 632 for Regida DP18 or 630
-- it seems reasonable to 
--for 32 3Times crossed we have:
--on the hub 4 interwhole intervals
--on the rim 6 rwhole intervals
--we assume the angle to be zero between the two spokes
--hub whole diameter 66mm
--hub width left=31.5+4/2 =33.5mm 
--hub spokewidth=52-4=48mm
--hub right =130.5-(33.5+48)=49mm  (for the other one
--we put y=0 in the center of the rim at 65.25 mm from the left 
--this puts the left hand side at 65.25-33.5=2=33.75 
--and the right hand side a 65.25-49=16.25
phiOneHub :: Double 
phiOneHub=4/2*360/(32/2)


--to store a spoking pattern we describe the 
type NumberOfPairsPerSide= Int
-- of the rim and the nubmer of 
type RimIntervals = Int
--between the rim points of a pair of CROSSED spokes
--that occupy 2 directly adjacent wholes in the hub
data Pattern = CrossedPattern NumberOfPairsPerSide RimIntervals 
                      deriving (Show)
--  examples
spline24 :: Pattern
spline24=CrossedPattern 6 10

threeCrossed32 :: Pattern
threeCrossed32 =CrossedPattern 8 10

numberOfHoles:: Pattern -> Int
numberOfHoles(CrossedPattern np _) = np*4
