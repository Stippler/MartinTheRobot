module Geometry
( 
   Circle(..)
 , CircleVec(..)
 , Vec(..)
 , toCircle
 , moveY
 , intersects
 , intersectsList
 , differenceVec
 , setRadius
 , rebound
 , move
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
} deriving (Show)

data BoolVec = BoolVec {
      bvBool :: Bool
    , bvVec :: Vec
}

tslf=10 -- time since last frame

toCircle :: Int -> Point2 Int  -> Circle
toCircle radius point = Circle (pointX point) (pointY point) radius

circleToVec :: Circle -> Vec
circleToVec circle = Vec (getX circle) (getY circle)

setRadius :: Int -> Circle -> Circle
setRadius r c = Circle (getX c) (getY c) r 

differenceVec :: Circle -> Circle -> Vec
differenceVec c1 c2 = Vec (getX c1 - getX c2) (getY c1 - getY c2)  

-- vector operations
addV :: Vec -> Vec -> Vec
addV (Vec x1 y1) (Vec x2 y2) = Vec (x1+x2) (y1+y2)

scalV :: Vec -> Int -> Vec
scalV (Vec x1 y1) scalar = Vec (x1*scalar) (y1*scalar)

dot :: Vec -> Vec -> Int
dot (Vec u v) (Vec x y) = (u*x) + (v*y)

norm :: (Num a, Floating a) => Vec -> a
norm v = sqrt.fromIntegral $ dot v v

-- movement
move :: CircleVec -> CircleVec
move (CircleVec (Circle x y r) (Vec vx vy)) = CircleVec (Circle (x+vx) (y+vy) r) (Vec vx vy)

moveAcc :: CircleVec -> CircleVec
moveAcc (CircleVec circle vec) = move (CircleVec circle (gravity vec)) 

gravity :: Vec -> Vec
gravity (Vec x y) = Vec x (y+1)

moveY :: Int -> Circle -> Circle
moveY dy (Circle x y r) = if y>500 then (Circle x (0-2*r+dy) r) else (Circle x (y+dy) r)


angle :: Vec -> Vec -> Float
angle v1 v2 = acos (fromIntegral (dot v1 v2) / (norm v1 * norm v2))

changeDir :: Vec -> Float -> Vec
changeDir (Vec x y) alpha = Vec u v
        where u = round (fromIntegral x * cos(alpha) + fromIntegral y * sin(alpha))  
              v = round (fromIntegral (-x) * sin(alpha) + fromIntegral y * cos(alpha))

-- collisions
intersects :: Circle -> Circle -> (Bool, Vec)
intersects c1 c2 = ((distance² c1 c2 <= (getRadius c1 + getRadius c2)^2), (differenceVec c1 c2))

--intersectsList :: [Circle] -> Circle -> Bool
--intersectsList circles c1 = or $ map fst (iii circles c1) 

--iii :: [Circle] -> Circle -> [(Bool, CircleVec)]
--iii circles c1 = zip (map bvBool bvList) $ zipWith CircleVec circles (map bvVec bvList)
--              where bvList = map (intersects c1) circles

-- change dir if intersect. for now: dir of raindrops assumed to be (0 1)
rebound :: [Circle] -> Circle -> [CircleVec]
--rebound circles c1 = zipWith CircleVec (map getCircle cvList) (zipWith changeDir (map getVec cvList) angles)
rebound circles c1 = zipWith CircleVec (circles) (zipWith (changeDir $ map snd bvList) angles)
              where bvList = map (intersects c1) circles
                    angles = fmap distinguish bvList
                    --cvList = map snd $ filter fst (iii circles c1) -- circles, which intersect
                    --angles = map (angle (Vec 0 (-1))) $ map getVec cvList -- desired angles for change
                    
distinguish :: (Bool, Vec) -> Vec
distinguish (b, v) = if b then (angle (Vec 0 (-1)) $ v) else (Vec 0 0)                    
                    

distance² :: Circle -> Circle -> Int
distance² c1 c2 = (getX c1 - getX c2)^2 + (getY c1 - getY c2)^2 








