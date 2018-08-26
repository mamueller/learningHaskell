module Main where
import System.IO (readFile)
import Data.Time (getCurrentTime)

-- for this to work OpenGL and GLUT have to be in firsProject.cabal
-- and stack build has to be called 
-- (I did stack install OpenGL before)
-- for this to worok the appropriate C-Libraries have to be installed
-- sudo apt-get install libgl1-mesa-dev libglu1-mesa-dev freeglut3-dev
-- for the graphics I followed https://wiki.haskell.org/OpenGLTutorial1
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

main :: IO ()
main = do
  time <- getCurrentTime
  putStrLn (greet "Markus" (show time) )

  --let (left,right)=spokeLengths aC32Narrow_dT411_32
  --putStrLn  ("aC32Narrow_dT411_32: \n left:" ++ (show left)   ++ "\nright:" ++ (show right)  )

  putStrLn ( spokeLengthStr aC32Wide_dT411_32)
  putStrLn ( spokeLengthStr aC32Narrow_dT411_32)
--  -- start opengle part
--  (_progName,args) <-getArgsAndInitialize
--  _window <- createWindow "Hello World"
--  displayCallback $= display
--  mainLoop

display :: DisplayCallback
display = do
  clear [ ColorBuffer ]
  -- renderPrimitive Points $
  -- renderPrimitive Triangles $
  -- renderPrimitive TriangleStrip$
  renderPrimitive TriangleFan$
    mapM_ (\(x,y,z) -> vertex $ Vertex3 x y z) myPoints
  flush

myPoints :: [(GLfloat,GLfloat,GLfloat)]
myPoints = [(sin (2*pi*k/12),cos (2*pi*k/12),0)| k<-[1..12]]

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
data Pattern = CrossedPattern { numberOfPairsPerSide::Int ,rimIntervals::Int }
               deriving (Show)
--  examples
spline24 :: Pattern
spline24=CrossedPattern 6 10

threeCrossed32 :: Pattern
threeCrossed32 =CrossedPattern 8 10

numberOfHubHoles:: Pattern -> Int
numberOfHubHoles(CrossedPattern np _) = np*4


data Flange =JBendFlange { flangeHoles::Int, flangeDiameter::Double, distance::Double} 
            |StraightPullFlange { flangeHoles::Int, flangeDiameter::Double, distance::Double, offSetFromMiddle::Double}
            deriving (Show)

type LeftFlange = Flange 
type RightFlange = Flange

data Hub = Hub {leftFlange::Flange, rightFlange::Flange, width::Double}
         deriving (Show)
           

aC32Wide= Hub {leftFlange=JBendFlange {flangeHoles=16 ,flangeDiameter=66.0,distance=33.5}
              ,rightFlange=JBendFlange {flangeHoles=16,flangeDiameter=66.0,distance=49.0}
              ,width= 130.0}

aC32Narrow= Hub {leftFlange=JBendFlange {flangeHoles=16 ,flangeDiameter=66.0,distance=42.0}
              ,rightFlange=JBendFlange {flangeHoles=16,flangeDiameter=66.0,distance=49.0}
              ,width= 130.0}


data Rim = Rim { rimHoles:: Int
                ,outerDiameter::Double  -- equal to the innermost diameter of the tyre, standardized 622mm for every 28"
                ,depth::Double -- from the outerDiameter to the bottom of the nipple seat (in the rim)  =distance from the spokewhole outer edge to the tyre seat
                ,rimOffSet::Double --0 for symmetric rims , distance of spoke holes to (z) center of rim                  
                }                        
dT411_32=Rim {rimHoles=32,outerDiameter=622,depth=13.0,rimOffSet=2}

erd :: Rim -> Double --the inner diameter of the rim plus the width of the rim material 
erd r  = (outerDiameter r )-(2*(depth r))

data Wheel = Wheel {hub::Hub, rim::Rim , pattern::Pattern, name::[Char]}
aC32Wide_dT411_32 = Wheel {hub=aC32Wide,rim=dT411_32,pattern=threeCrossed32,name="aC32Wide_dT411_32"}
aC32Narrow_dT411_32 = Wheel {hub=aC32Narrow,rim=dT411_32,pattern=threeCrossed32,name="aC32Narrow_dT411_32" }

zDists :: Hub -> (Double,Double) --compute the distance of the flanges from the center of the hub
zDists (Hub leftFlange rightFlange width)  = ( hw-(distance leftFlange) , hw-(distance rightFlange) )
    where hw=width/2

data Coords =  CylinderCoords{r::Double,phi::Double,z::Double}
               |CartesianCoords{x::Double,y::Double,z::Double} 
               deriving(Show)

p1:: Coords
p1= CartesianCoords 1 1 1 
pc1= CylinderCoords (sqrt 2) (pi/4) 1 

norm ::Coords ->Double
norm ( CartesianCoords x y z )  = sqrt ( x^2 + y^2 + z^2 )
--norm ( CylinderCoords r phi z ) = norm (toCartesian ( CylinderCoords r phi z ))
norm c = norm (toCartesian c)

toCartesian :: Coords->Coords 
--toCartesian ( CartesianCoords x y z)  = CartesianCoords x y z
toCartesian ( CylinderCoords r phi z ) = CartesianCoords x y z
    where   x=r*cos phi
            y=r*sin phi
toCartesian x = x --identety
                                                

pointDistance :: Coords-> Coords-> Double 
pointDistance (CartesianCoords x1 y1 z1 ) (CartesianCoords x2 y2 z2 )= norm (CartesianCoords (x2-x1) (y2-y1) (z2-z1) )
pointDistance p1 p2 = pointDistance (toCartesian p1) (toCartesian p2)

spokeLengthStr :: Wheel -> [Char]
spokeLengthStr wheel  = 
    let 
        (left,right)=spokeLengths wheel
    in "\nWheel:" ++ (name wheel) ++ "\n left:" ++ (show left)   ++ "\nright:" ++ (show right)  
  
        

spokeLengths :: Wheel -> (Double,Double)
spokeLengths (Wheel hub rim pattern _ )=(leftSpokeLength,rightSpokeLength)
    where   leftSpokeLength=pointDistance hubPointLeft rimPoint
            rightSpokeLength=pointDistance hubPointRight rimPoint
            lf=leftFlange hub
            rf=rightFlange hub
            
            delta_phi_lf=(2*pi) / fromIntegral(flangeHoles lf) 
            delta_phi_rf=(2*pi) / fromIntegral(flangeHoles rf) 

            (zl,zr)=zDists hub
            hubPointLeft=CylinderCoords{
                 r=(flangeDiameter lf)/2
                ,phi=delta_phi_lf/2
                ,z=zl-(rimOffSet rim)}
            
            hubPointRight=CylinderCoords{
                 r=(flangeDiameter rf)/2
                ,phi=delta_phi_rf/2
                ,z=zr+(rimOffSet rim)
            }
            delta_phi_rim=(2*pi) / fromIntegral ( numberOfHubHoles pattern)
            rimPoint=CylinderCoords{
                 r=(erd rim)/2
                ,phi= -(delta_phi_rim*fromIntegral (rimIntervals pattern)/2)
                ,z=0 
            }


greet:: [Char]->[Char]->[Char]
greet name timeStr = "Hello " ++ name ++ "! It is now" ++ timeStr 


printConfig =do 
    contents <-readFile "stack.yaml"
    putStrLn contents



