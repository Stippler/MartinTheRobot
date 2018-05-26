module Geometry
( 
   Circle(..)
 , toCircle
 , moveY
 , intersects
 , intersectsList
 , fallingWithGravity
 , differenceVector
) where
import Graphics.UI.WX.Types

data Circle = Circle {
      getX :: Int
    , getY :: Int
    , getRadius :: Int
} deriving (Show)
      
data Vec = Vec {
      xx :: Int
    , yy :: Int
} deriving (Show)

data CircleVec = CircleVec {
      getCircle :: Circle
    , getVec :: Vec
}

tslf=10

addV :: Vec -> Vec -> Vec
addV (Vec x1 y1) (Vec x2 y2) = Vec (x1+x2) (y1+y2)

scalV :: Vec -> Int -> Vec
scalV (Vec x1 y1) scalar = Vec (x1*scalar) (y1*scalar)

move :: CircleVec -> CircleVec
move CircleVec (Circle x y r) (Vec vx vy) = CircleVec (Circle (x+vx) (y+vy)) (Vec vx vy)

moveAcc :: CircleVec -> CircleVec
moveAcc CircleVec circle vec=move (CircleVec circle (gravity vec)) 

gravity :: Vec -> Vec
gravity Vec x y = Vec x (y+1)

toCircle :: Int -> Point2 Int  -> Circle
toCircle radius point = Circle (pointX point) (pointY point) radius

moveY :: Int -> Circle -> Circle
moveY dy (Circle x y r) = if y>500 then (Circle x (0-2*r+dy) r) else (Circle x (y+dy) r)

intersects :: Circle -> Circle -> Bool
intersects c1 c2 = distance² c1 c2 <= (getRadius c1 + getRadius c2)^2

intersectsList :: [Circle] -> Circle -> Bool
intersectsList circles c1 = any (intersects c1) circles

distance² :: Circle -> Circle -> Int
distance² c1 c2 = (getX c1 - getX c2)^2 + (getY c1 - getY c2)^2

-- SIGNATUR VERÄNDERN ?
fallingWithGravity :: Circle -> Int -> Int -> Circle
fallingWithGravity (Circle x y r) t0 t = Circle x (y+5*(t-t0)^2) r

differenceVector :: Circle -> Circle -> Point2 Int
differenceVector c1 c2 = Point (getX c1 - getX c2) (getY c1 - getY c2) 

-- 
rebound :: Circle -> Circle -> Point2 Int
rebound shot drop = changeDir velDrop (-1)*alpha -- + "fallverhalten" des regentropfens
	where d = differenceVector drop shot
	  alpha = angle (differenceVector shot drop) drop 
	      -- velDrop = velocity of raindrop

dot :: Point2 Int -> Point2 Int -> Int
dot p1 p2 = (pointX p1 * pointX p2) + (pointY p1 * pointY p2)

-- Signatur wird nicht passen
angle :: Point2 Int -> Point2 Int -> Float
angle v1 v2 = acos(dot v1 v2 / (sqrt $ dot v1 v1)*(sqrt $ dot v2 v2))

changeDir :: Point2 Int -> Float -> Point2 Int
changeDir (Point x y) alpha = Point (x*cos(alpha) + y*sin(alpha)) (-x*sin(alpha) + y*cos(alpha))










