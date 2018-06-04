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
width = 800
height = 600

distVec :: Circle -> Circle -> Vec
distVec c1 c2 = Vec (c1^.x - c2^.x) (c1^.y - c2^.y)  

-- vector operations
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

-- movement
move :: CircleVec -> CircleVec
--move (CircleVec circle vec) = CircleVec (circle.x +~ vec^.)
move cv = cv & circle.x +~ (cv^.vec^.vx) & circle.y  +~ (cv^.vec^.vy) 

moveAcc :: Float -> CircleVec -> CircleVec
moveAcc gravity cv = (vec.vy +~ gravity) . move $ cv

--moveY :: Float -> Circle -> Circle
--moveY dy (Circle x y r) = if y>500 then (Circle x (0-2*r+dy) r) else (Circle x (y+dy) r)

angle :: Vec -> Vec -> Float
angle v1 v2 = acos ( (dot v1 v2) / (norm v1 * norm v2))

changeDir :: Vec -> Float -> Vec
changeDir (Vec x y) alpha = Vec u v
        where u = x * cos(alpha) +  y * sin(alpha)
              v = (-x) * sin(alpha) +  y * cos(alpha)

-- collisions
intersects :: Circle -> Circle -> Bool 
intersects c1 c2 = (distance² c1 c2 <= (c1^.r + c2^.r)^2)

collision :: ([CircleVec], [CircleVec]) -> ([CircleVec], [CircleVec])
collision (shots, drops) =  (filter (not . (intersectsList drops)) shots, collision12 drops shots)

collision12 :: [CircleVec] -> [CircleVec] -> [CircleVec]
collision12 drops [] = drops
collision12 drops (x:xs) = collision12 (collision1 drops (x^.circle)) xs

collision1 :: [CircleVec] -> Circle -> [CircleVec]
collision1 circlevecs c = map (\ drop -> if intersects c $ drop^.circle then collision2 drop c else drop) circlevecs 

collision2 :: CircleVec -> Circle -> CircleVec
collision2 cv c = cv & vec %~ (addV $ (normed v) `scalV` 3)
           where v = distVec (cv^.circle) c

intersectsList :: [CircleVec] -> CircleVec -> Bool 
intersectsList circles c1 = any ((intersects (c1^.circle)) . _circle) circles 

distance² :: Circle -> Circle -> Float
distance² c1 c2 = dx*dx + dy * dy 
       where dx = c1^.x - c2^.x
             dy = c1^.y - c2^.y 
