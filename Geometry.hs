module Geometry
( 
   Circle(..)
 , toCircle
 , moveY
 , intersects
 , intersectsList
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
moveY dy (Circle x y r) = if y>500 then (Circle x (0-2*r+dy) r) else (Circle x (y+dy) r)

intersects :: Circle -> Circle -> Bool
intersects c1 c2 = distance² c1 c2 <= (getRadius c1 + getRadius c2)^2

intersectsList :: [Circle] -> Circle -> Bool
intersectsList circles c1 = any (intersects c1) circles

distance² :: Circle -> Circle -> Int
distance² c1 c2 = (getX c1 - getX c2)^2 + (getY c1 - getY c2)^2

-- 
