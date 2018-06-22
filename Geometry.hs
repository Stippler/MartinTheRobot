{-# LANGUAGE TemplateHaskell #-}

module Geometry
( 
   Circle(..)
 , x
 , y
 , r
 , CircleVec(..)
 , circle
 , vec
 , Vec(..)
 , vx
 , vy
 , move -- for Object.updateShots
 , moveAcc -- for Object.updateDrops 
 , intersectsList -- needed in Object, i. e. filterShots
 , iterates -- Object.collisionoccured
 , width
 , height
 , normed 
 , addV
 , scalV
 , distVec
 , intersects
) where

import Control.Lens
import Data.Function
import Data.Fixed (mod')

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
-- size of window
width = 800
height = 600

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

-- | returns normal vector of v
normalVec :: Vec -> Vec
normalVec v = Vec ((-1)*v^.vy) (v^.vx)

-----------------------
-- circle operations --
-----------------------

-- | distance between two circles squared
distance² :: Circle -> Circle -> Float
distance² c1 c2 = dx*dx + dy * dy 
       where dx = c1^.x - c2^.x
             dy = c1^.y - c2^.y

-- | takes a list of circles and a circle c1 and returns True, if c1 intersects any of the circles from the list
intersectsList :: [CircleVec] -> Circle -> Bool
intersectsList circles c1 = any ((intersects (c1)) . _circle) circles

-- | returns the vector from the center of the 2nd circle to the center of the 1st one
distVec :: Circle -> Circle -> Vec
distVec c1 c2 = Vec (c1^.x - c2^.x) (c1^.y - c2^.y)  

--------------
-- movement --
--------------

-- | moves a CircleVec (c, v) by adding vector v to the center of the circle c; if c leaves the frame on the left or right side, it enters on the other one; if it leaves the frame on the bottom, it enters on the top again
move :: CircleVec -> CircleVec
move cv =  if yy > Geometry.height 
               then cv & circle.x .~ (mod' (vv^.vx + xx + 373) width) & circle.y .~ (-1)*cv^.circle^.r & vec .~ (Vec 0 0)
               else cv & circle.x .~ (mod' (vv^.vx + xx) width) & circle.y .~ (vv^.vy + yy)
                   where xx = cv^.circle^.x
                         yy = cv^.circle^.y
                         vv = cv^.vec 


-- | calls move and adds an acceleration to the y vector afterwards (used for gravity)
moveAcc :: Float -> CircleVec -> CircleVec
moveAcc acc cv = (vec.vy +~ acc) . move $ cv

-- | calculates the angle between two vectors
angle :: Vec -> Vec -> Float
angle v1 v2 = acos ( (dot v1 v2) / (norm v1 * norm v2))

-- | changes the direction of a vector by an angle
changeDir :: Vec -> Float -> Vec
changeDir (Vec x y) alpha = Vec u v
        where u =   x  * cos(alpha) +  y * sin(alpha)
              v = (-x) * sin(alpha) +  y * cos(alpha)

---------------
-- collision --
---------------

-- | checks if two circles overlap
intersects :: Circle -> Circle -> Bool
intersects c1 c2 = (distance² c1 c2 <= (c1^.r + c2^.r)^2)

-- | iterates over the 2nd list of CircleVecs checks collisions with the first list
-- TODO name of parameters?
iterates :: [CircleVec] -> [CircleVec] -> (CircleVec -> Circle -> CircleVec)-> [CircleVec]
iterates drops [] _ = drops
iterates drops (x:xs) func = iterates (checkCollision drops (x^.circle) func) xs func
 
-- | checks whether or not there is an collision, if there is one it executes the function collisionOccured
checkCollision :: [CircleVec] -> Circle -> (CircleVec -> Circle -> CircleVec) -> [CircleVec] -- add function f :: CircleVec -> Circle -> CircleVec
checkCollision circlevecs c func = map (\ circlevec -> if intersects c $ circlevec^.circle then func circlevec c else circlevec) circlevecs

-- THIS WILL BE REMOVED
-- | changes the first parameter accordingly
--collisionOccured :: CircleVec -> Circle -> CircleVec
--collisionOccured cv c = cv & vec %~ (addV $ (normed v) `scalV` 3)
--           where v = distVec (cv^.circle) c
 
