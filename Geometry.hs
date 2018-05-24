module Geometry
( 
   Circle(..)
 , toCircle
 , moveY
 , intersects
) where
import Graphics.UI.WX.Types
data Circle = Circle {
      getX :: Int
    , getY :: Int
    , getRadius :: Int
} deriving (Show)


toCircle :: Int -> Point2 Int  -> Circle
toCircle radius point = Circle (pointX point) (pointY point) radius

moveY :: Int -> Circle -> Circle
moveY dy (Circle x y r) = (Circle x (y+dy) r)

intersects :: Circle -> Circle -> Bool
intersects c1 c2 = distance² c1 c2 <= (getRadius c1 + getRadius c2)^2

distance² :: Circle -> Circle -> Int
distance² c1 c2 = (getX c1 - getX c2)^2 + (getY c1 - getY c2)^2


