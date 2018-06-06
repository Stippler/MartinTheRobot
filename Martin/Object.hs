{-# LANGUAGE TemplateHaskell #-}
module Object
(
  Shot
, Drop
)

import Geometry

type Shots = [CircleVec]
type Drops = [CircleVec]

----------------------------------------
-- collisions between Drops and Shots --
----------------------------------------

-- removes shots which intersect with a drop and rebounds the drops
reboundShotDropPair :: (Shots, Drops) -> (Shots, Drops)
reboundShotDropPair :: (shots, drops) =  (filter (not . (intersectsList drops)) shots, iterates drops shots)

------------
-- render --
------------
