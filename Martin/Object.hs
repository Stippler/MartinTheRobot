{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Object
(
  Shots
, Drops
) where

import Geometry
import Graphics.UI.WX
import Paths (getDataFile)


type Shot = CircleVec
type Drop = CircleVec
type Martin = CircleVec

type Shots = [Shot]
type Drops = [Drop]


----------------------------------------
-- collisions between Drops and Shots --
----------------------------------------

-- removes shots which intersect with a drop and rebounds the drops
reboundShotDropPair :: (Shots, Drops) -> (Shots, Drops)
reboundShotDropPair :: (shots, drops) =  (filter (not . (intersectsList drops)) shots, iterates drops shots)




------------
-- render --
------------

-- TODO scaling of the image: https://stackoverflow.com/questions/7270956/draw-a-scaled-bitmap-using-wxhaskell

dropImage, shotImage, martinImage :: Bitmap ()
dropImage = bitmap $ getDataFile "m2r2.png"
shotImage = bitmap $ getDataFile "m2r2.png"
martinImage = bitmap $ getDataFile "m2r2.png"

renderShot :: DC a -> Shot -> IO ()
renderShot dc shot = drawBitmap dc shotImage (toPoint shot) True []

renderMartin :: DC a -> Martin -> IO ()
renderMartin dc martin = drawBitmap dc martinImage (toPoint martin) True []

renderDrop :: DC a -> Drop -> IO ()
renderDrop dc drop = drawBitmap dc dropImage (toPoint drop) True []

toPoint :: CircleVec -> Point
toPoint cv = Point (cv & circle^.x) (cv & circle^.y)



