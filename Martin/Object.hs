{-# LANGUAGE TemplateHaskell #-}
module Object
(
  Shot
, Drop
)

import Geometry

type Shot = CircleVec
type Drop = CircleVec

----------------------------------------
-- collisions between Drops and Shots --
----------------------------------------

-- removes shots which intersect with a drop and rebounds the drops
reboundShotDropPair :: ([Shot], [Drop]) -> ([Shot], [Drop])
reboundShotDropPair :: (shots, drops) =  (filter (not . (intersectsList drops)) shots, collision12 drops shots)




