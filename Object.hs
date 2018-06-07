{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Object
(
  Shots
, Drops
, Martin
, initialMartin
, initialDrops
, updateDropShotPair
, reboundShotDropPair
, addShotToList
, updateMartin
, addShot
, render
) where

import Geometry
import Graphics.UI.WX
import Graphics.UI.WXCore

--import Paths (getDataFile)
import Data.Function
import Control.Lens hiding (set)



---------------
-- Constants --
---------------
shotRadius = 5
shotSpeed = Vec 0 (-5)
martinRadius = 15

------------
-- Martin --
------------

type Martin = Circle

initialMartin :: Martin
initialMartin = Circle 0 0 0

updateMartin :: Point2 Int  -> Martin
updateMartin point = Circle (fromIntegral (pointX point)) (fromIntegral (pointY point)) martinRadius

----------
-- Shot -- 
----------

type Shot = CircleVec
type Shots = [Shot]

addShot :: Maybe Martin -> (Shots, Drops) -> (Shots, Drops)
addShot c (shots, drops) = (addShotToList c shots, drops)

addShotToList :: Maybe Circle -> Shots -> Shots
addShotToList Nothing s = s
addShotToList (Just c) s = (generateShot (c^.x) (c^.y)):s

generateShot :: Float -> Float -> Shot
generateShot x y = CircleVec (Circle x y shotRadius) shotSpeed

----------
-- Drop --
----------

type Drop = CircleVec
type Drops = [Drop]

initialDrops :: Drops
initialDrops = [--CircleVec (Circle 10 20 64) (Vec 0 0),
                --CircleVec (Circle 60 00 64) (Vec 0 0),
                --CircleVec (Circle 110 (-20) 64) (Vec 0 0),
                CircleVec (Circle 160 (-40) 64) (Vec 0 0)]

---------------------
-- Drops and Shots --
---------------------

updateDropShotPair :: (Shots, Drops) -> (Shots, Drops)
updateDropShotPair (shots, drops) = reboundShotDropPair (map move shots, map (moveAcc 0.005) drops) 

-- removes shots which intersect with a drop and rebounds the drops
reboundShotDropPair :: (Shots, Drops) -> (Shots, Drops)
reboundShotDropPair (shots, drops) =  (filter (not . ( (||) <$> (intersectsList drops) <*> (\ c -> c^.y < (- c^.r))._circle )) shots, iterates drops shots)



------------
-- render --
------------

-- TODO scaling of the image: https://stackoverflow.com/questions/7270956/draw-a-scaled-bitmap-using-wxhaskell

dropImage = bitmap $ "m2r2.png"
shotImage = bitmap $ "m2r2.png"
martinImage = bitmap $ "m2r2.png"

render :: Martin -> (Shots, Drops) -> DC a -> Rect -> IO ()
render martin (shots, drops) dc viewArea = do
  scaleDC dc martin
  renderMartin dc martin
  resetScaleDC dc
  mapM ((renderShot dc)) shots 
  mapM ((renderDrop dc)) drops
  return ()

renderCircle :: DC a -> Geometry.Circle -> IO ()
renderCircle dc c = do
  Graphics.UI.WX.circle dc (point (round $ c^.x) (round $ c^.y)) (round $ c^.r) []

renderShot :: DC a -> Shot -> IO ()
renderShot dc shot = do
  scaleDC dc (shot^.Geometry.circle)
  drawBitmap dc shotImage (toPoint $ shot^.Geometry.circle) True []
  resetScaleDC dc
  return ()

renderMartin :: DC a -> Martin -> IO ()
renderMartin dc martin = drawBitmap dc martinImage (toPoint martin) True []

renderDrop :: DC a -> Drop -> IO ()
renderDrop dc drop = do 
  scaleDC dc (drop^.Geometry.circle)
  drawBitmap dc dropImage (toPoint $ drop^.Geometry.circle) True []
  resetScaleDC dc
  return ()

scaleDC :: DC a -> Circle -> IO ()
scaleDC dc circle = do
  let scale=((realToFrac $ circle^.r*2)/64)
  dcSetUserScale dc scale scale
  return ()

resetScaleDC :: DC a -> IO ()
resetScaleDC dc = do
  dcSetUserScale dc 1 1
  return ()


toPoint :: Circle -> Point
toPoint c = Point (round $ (c^.x- (c^.r)) / (c^.r*2/64)) (round $ (c^.y - (c^.r)) / (c^.r*2/64))