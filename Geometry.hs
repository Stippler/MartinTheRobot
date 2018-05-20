module Geometry
( 
   Circle(..)
 , toCircle
 , moveY
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
