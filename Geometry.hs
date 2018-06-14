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
 , move
 , moveAcc 
 , intersectsList
 , iterates
 , width
 , height
 , normed -- for exporting collisionOccured
 , addV
 , scalV
 , distVec
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

-- returns normal vector of v
normalVec :: Vec -> Vec
normalVec v = Vec ((-1)*v^.vy) (v^.vx)

-----------------------
-- circle operations --
-----------------------

-- distance between two circles pow 2
distance² :: Circle -> Circle -> Float
distance² c1 c2 = dx*dx + dy * dy 
       where dx = c1^.x - c2^.x
             dy = c1^.y - c2^.y

intersectsList :: [CircleVec] -> Circle -> Bool
intersectsList circles c1 = any ((intersects (c1)) . _circle) circles

distVec :: Circle -> Circle -> Vec
distVec c1 c2 = Vec (c1^.x - c2^.x) (c1^.y - c2^.y)  

--------------
-- movement --
--------------

-- moves a CircleVec by adding vx and vy of vec to x and y of the circles; window is almost a torus
-- TODO: if more than 1 drops, change signature to also take some number from a list of 'offset-numbers' like 373 below
move :: CircleVec -> CircleVec
move cv =  if yy > Geometry.height 
               then cv & circle.x .~ (mod' (vv^.vx + xx + 373) width) & circle.y .~ (-1)*cv^.circle^.r & vec .~ (Vec 0 0)
               else cv & circle.x .~ (mod' (vv^.vx + xx) width) & circle.y .~ (vv^.vy + yy)
                   where xx = cv^.circle^.x
                         yy = cv^.circle^.y
                         vv = cv^.vec 

{-
moveX :: Circle -> Float -> Float
moveX circle moveX = if right < 0 then Geometry.width+r else (if left>Geometry.width then (-r) else cx+moveX) 
    where r = circle^.radius
          cx = circle^.x
          right = cx+r+moveX -- right side of the circle after moving
          left  = cx-r+moveX -- left side of the circle

moveY :: Circle -> Float -> Circle
moveY circle moveY = if top > Geometry.height then Circle (circle^.x) (-r) r else circle & y +~ moveY
    where r = circle^.radius
          y = circle^.y
          top = circle^.y-r+moveY

-}

-- calls move and adds an acceleration to the y vector afterwards (used for gravity)
moveAcc :: Float -> CircleVec -> CircleVec
moveAcc acc cv = (vec.vy +~ acc) . move $ cv

-- calculates the angle between two vectors
angle :: Vec -> Vec -> Float
angle v1 v2 = acos ( (dot v1 v2) / (norm v1 * norm v2))

-- changes the direction of a vector by an angle
changeDir :: Vec -> Float -> Vec
changeDir (Vec x y) alpha = Vec u v
        where u =   x  * cos(alpha) +  y * sin(alpha)
              v = (-x) * sin(alpha) +  y * cos(alpha)

---------------
-- collision --
---------------

-- checks if two circles overlap
intersects :: Circle -> Circle -> Bool
intersects c1 c2 = (distance² c1 c2 <= (c1^.r + c2^.r)^2)

-- iterates over the list of CircleVecs (second parameter) and passes one element of the second parameter and the first parameter to checkCollision
-- TODO add function: (CircleVec -> Circle -> CircleVec) -> 
iterates :: [CircleVec] -> [CircleVec] -> (CircleVec -> Circle -> CircleVec)-> [CircleVec]
iterates drops [] _ = drops
iterates drops (x:xs) func = iterates (checkCollision drops (x^.circle) func) xs func
 
-- checks whether or not there is an collision, if there is one it executes the Function collisionOccured
checkCollision :: [CircleVec] -> Circle -> (CircleVec -> Circle -> CircleVec) -> [CircleVec] -- add function f :: CircleVec -> Circle -> CircleVec
checkCollision circlevecs c func = map (\ circlevec -> if intersects c $ circlevec^.circle then func circlevec c else circlevec) circlevecs

-- THIS WILL BE REMOVED
-- changes the first parameter accordingly
collisionOccured :: CircleVec -> Circle -> CircleVec
collisionOccured cv c = cv & vec %~ (addV $ (normed v) `scalV` 3)
           where v = distVec (cv^.circle) c
 
