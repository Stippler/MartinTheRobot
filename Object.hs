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
, updateBackground
, playMusic
, addDrop
, collisionOccured
, intersectsMartin
) where

import Geometry
import Graphics.UI.WX
import Graphics.UI.WXCore
import Data.Function
import Control.Lens hiding (set)



---------------
-- Constants --
---------------
shotRadius = 25
shotSpeed = Vec 0 (-5) 
martinRadius = 15 
dropAcc=0.1
bgSpeed=3
dropRadius=50

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

addShot :: Martin -> (Shots, Drops) -> (Shots, Drops)
addShot c (shots, drops) = (addShotToList c shots, drops)

addShotToList :: Circle -> Shots -> Shots
addShotToList c s = (generateShot (c^.x) (c^.y)):s

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

addDrop :: Float -> (Shots, Drops) -> (Shots, Drops) 
addDrop random (shots, drops) = (shots, generateDrop random : drops)

generateDrop :: Float -> CircleVec
generateDrop random = CircleVec (Circle (random*Geometry.width) (-dropRadius) $ dropRadius*random) (Vec 0 0) 

----------------------
-- Drops and Martin --
----------------------

intersectsMartin :: (Shots, Drops) -> Martin -> Bool
intersectsMartin (_, drops) martin = intersectsList drops martin


---------------------
-- Drops and Shots --
---------------------

updateDropShotPair :: (Shots, Drops) -> (Shots, Drops)
updateDropShotPair (shots, drops) = reboundShotDropPair (map move shots, map (moveAcc dropAcc) drops) collisionOccured

-- removes shots which intersect with a drop and rebounds the drops
reboundShotDropPair :: (Shots, Drops) -> (CircleVec -> Circle -> CircleVec) -> (Shots, Drops)
reboundShotDropPair (shots, drops) func =  (filter (not . ( (||) <$> (intersectsList drops)._circle <*> (\ c -> c^.y < (- c^.r))._circle )) shots, iterates drops shots func)


----------------
-- background --
----------------

updateBackground :: (Int, Int) -> (Int, Int)
updateBackground (pos, bgCount) = if pos<round (Geometry.height*2) then (pos+bgSpeed,bgCount) else (0, bgCount+1)

------------
-- render --
------------

-- TODO scaling of the image: https://stackoverflow.com/questions/7270956/draw-a-scaled-bitmap-using-wxhaskell

-- background changed: kathi june 13th
dropImage, shotImage, martinImage, bgImage :: Bitmap ()
dropImage = bitmap $ "drop.png"
shotImage = bitmap $ "shot.png"
martinImage = bitmap $ "m2r21.png"
bgImage = bitmap $ "background3.png"

render :: Martin -> (Shots, Drops) -> (Int,Int) -> Bool -> DC a -> Rect -> IO ()
render martin (shots, drops) bgPos shooting dc viewArea = do
  renderBackground dc bgPos
  scaleDC dc martin
  renderMartin dc martin
  resetScaleDC dc
  mapM ((renderShot dc)) shots 
  mapM ((renderDrop dc)) drops
  return ()

renderCircle :: DC a -> Geometry.Circle -> IO ()
renderCircle dc c = do
  Graphics.UI.WX.circle dc (point (round $ c^.x) (round $ c^.y)) (round $ c^.r) []

renderBackground :: DC a -> (Int,Int) -> IO ()
renderBackground dc pos = do 
  drawBitmap dc bgImage (Point 0 $ fst pos) True []
  drawBitmap dc bgImage (Point 0 $ fst pos - round Geometry.height*2) True []
  return ()

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

-----------
-- sound --
-----------

music :: Sound ()
music = sound "music2.wav"
shotSound = sound "pew.wav"

playShot :: IO ()
playShot = play music--return ()


playMusic :: IO ()
playMusic = play music

-------------
-- testing --
-------------
collisionOccured :: Shot -> Martin -> Shot
collisionOccured cv c = cv & Geometry.vec %~ (addV $ (normed v) `scalV` 5)
            where v = distVec (cv^.Geometry.circle) c
