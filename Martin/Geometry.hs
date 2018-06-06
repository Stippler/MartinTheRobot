{-# LANGUAGE TemplateHaskell #-}

module Geometry
( 
   Circle(..)
 , x
 , y
 , r
 , CircleVec(..)
 , Vec(..)
 , move
 , moveAcc
 , collision
) where

import Control.Lens
import Data.Function

data CircleVec = CircleVec {
      _circle :: Circle
    , _vec :: Vec
} deriving (Show)



data Circle = Circle {
      _x :: Float
    , _y :: Float
    , _r :: Float
} deriving (Show)
      
data Vec = Vec {
      _vx :: Float
    , _vy :: Float
} deriving (Show)

makeLenses ''CircleVec
makeLenses ''Circle
makeLenses ''Vec

tslf = 10 -- time since last frame

-----------------------
-- vector operations --
-----------------------
addV :: Vec -> Vec -> Vec
addV v1 v2 = Vec (v1^.vx+v2^.vx) (v1^.vy+v2^.vy)

scalV :: Vec -> Float -> Vec
scalV v scalar = Vec (v^.vx*scalar) (v^.vy*scalar)

dot :: Vec -> Vec -> Float
dot v1 v2 = (v1^.vx*v2^.vx) + (v1^.vy*v2^.vy)

norm :: Vec -> Float
norm v = sqrt $ dot v v

normed :: Vec -> Vec
normed v = scalV v (1/length)
    where length = norm v

normalVec
-----------------------
-- circle operations --
-----------------------

-- distance between two circles pow 2
distance² :: Circle -> Circle -> Float
distance² c1 c2 = dx*dx + dy * dy 
       where dx = c1^.x - c2^.x
             dy = c1^.y - c2^.y

intersectsList :: [CircleVec] -> CircleVec -> Bool
intersectsList circles c1 = any ((intersects (c1^.circle)) . _circle) circles

distVec :: Circle -> Circle -> Vec
distVec c1 c2 = Vec (c1^.x - c2^.x) (c1^.y - c2^.y)  

--------------
-- movement --
--------------

-- moves a CircleVec by adding vx and vy of vec to x and y of the circles
move :: CircleVec -> CircleVec
move cv = cv & circle.x +~ (cv^.vec^.vx) & circle.y  +~ (cv^.vec^.vy) 

-- calls move and adds an acceleration to the y vector afterwards (used for gravity)
moveAcc :: Float -> CircleVec -> CircleVec
moveAcc acc cv = (vec.vy +~ acc) . move $ cv

-- calculates the angle between two vectors
angle :: Vec -> Vec -> Float
angle v1 v2 = acos ( (dot v1 v2) / (norm v1 * norm v2))

-- changes the direction of a vector by a angle
changeDir :: Vec -> Float -> Vec
changeDir (Vec x y) alpha = Vec u v
        where u = x * cos(alpha) +  y * sin(alpha)
              v = (-x) * sin(alpha) +  y * cos(alpha)

---------------
-- collision --
---------------

-- checks if two circles overlap
intersects :: Circle -> Circle -> Bool
intersects c1 c2 = (distance² c1 c2 <= (c1^.r + c2^.r)^2)

-- iterates over the list of CircleVecs (second parameter) and passes one element of the second parameter and the first parameter to checkCollision
iterates :: [CircleVec] -> [CircleVec] -> [CircleVec]
iterates drops [] = drops
iterates drops (x:xs) = iterates (checkCollision drops (x^.circle)) xs

-- checks wheather or not there is an collision, if there is one it executes the Function collisionOccured
checkCollision :: [CircleVec] -> Circle -> [CircleVec]
checkCollision circlevecs circle = map (\ circlevec -> if intersects circle $ circlevec^.circle then collisionOccured circlevec c else circlevec) circlevecs

-- changes the first parameter accordingly
collisionOccured :: CircleVec -> Circle -> CircleVec
collisionOccured cv c = cv & vec %~ (addV $ (normed v) `scalV` 3)
           where v = distVec (cv^.circle) c












