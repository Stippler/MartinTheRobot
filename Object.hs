{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Object
(
  Shots
, Drops
, Martin
, initialMartin
, initialDrops
, updateDrops
, updateMartin
, addShot
, render
, updateBackground
, addDrop
, collisionOccured
, intersectsMartin
, updateShots
, intersectionShotDrop
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
bgSpeed=5
dropRadius=100
minSizeDrop=10

------------
-- Martin --
------------

type Martin = Circle

initialMartin :: Martin
initialMartin = Circle 0 0 0

updateMartin :: Point2 Int  -> Martin
updateMartin point = Circle (fromIntegral (pointX point)) (fromIntegral (pointY point)) martinRadius

intersectsMartin :: Drops -> Martin -> Bool
intersectsMartin drops martin = intersectsList drops martin

----------
-- Shot -- 
----------

type Shot = CircleVec
type Shots = [Shot]

-- | appends a shot at (x, y), the center of c, to the beginning of the list of shots s 
addShot :: Circle -> Shots -> Shots
addShot c s = (generateShot (c^.x) (c^.y)):s

-- | generates a shot at point (x, y)
generateShot :: Float -> Float -> Shot
generateShot x y = CircleVec (Circle x y shotRadius) shotSpeed

-- | moves shots according to their motion vectors and 
updateShots ::  Drops -> Shots -> Shots
updateShots drops shots =  filterShots (map move shots) $ drops

-- | removes any shot from shots = [shot] which intersect with any drop from drops = [drop]
-- |                                   or which have left the frame, their y-coordinate being too small
filterShots :: Shots -> Drops -> Shots
filterShots shots drops = filter (not . ( (||) <$> (intersectsList drops)._circle <*> (\ c -> c^.y < (- c^.r))._circle )) shots

----------
-- Drop --
----------

type Drop = CircleVec
type Drops = [Drop]

initialDrops :: Drops
initialDrops = [CircleVec (Circle 160 (40) 64) (Vec 0 0)]

-- | appends a new drop at (x = random) just above the frame
addDrop :: Float -> Drops -> Drops 
addDrop random drops = generateDrop random : drops

-- | generates a new drop at a (x = random*width) just above the frame
-- TODO maybe 2 randoms
generateDrop :: Float -> CircleVec
generateDrop random = CircleVec (Circle (random*Geometry.width) (-dropRadius) $ dropRadius*random+minSizeDrop) (Vec 0 0) 

-- | moves all drops and changes their direction, if they intersect with any shot in shots
updateDrops :: Shots -> Drops -> Drops
updateDrops shots drops = iterates (map (moveAcc dropAcc) drops) shots collisionOccured

-- | changes motion vector of cv based on difference vector between cv's and c's center
collisionOccured :: Drop -> Circle -> Drop
collisionOccured cv c = cv & Geometry.vec %~ (addV $ (normed v) `scalV` 5)
            where v = distVec (cv^.Geometry.circle) c

----------------
-- background --
----------------

-- | updates the background according to the specified speed, i. e. how fast the background moves
updateBackground :: (Int, Int) -> (Int, Int)
updateBackground (pos, bgCount) = if pos<round (Geometry.height*2) then (pos+bgSpeed,bgCount) else (0, bgCount+1)

------------
-- render --
------------

-- TODO scaling of the image: https://stackoverflow.com/questions/7270956/draw-a-scaled-bitmap-using-wxhaskell
 
dropImage, shotImage, martinImage, bgImage :: Bitmap ()
dropImage = bitmap $ "drop.png"
shotImage = bitmap $ "shotSquare.png"
martinImage = bitmap $ "m2r21.png"
bgImage = bitmap $ "background3.png"

-- | renders image, where
-- |                martin, shots and drops ... TODO fixme
-- |                bgPos ... TODO fixme
render :: Martin -> Shots -> Drops -> (Int,Int) -> Int ->  DC a -> Rect -> IO ()
render martin shots drops bgPos score dc viewArea = do
  renderBackground dc bgPos
  scaleDC dc martin
  renderMartin dc martin
  resetScaleDC dc
  mapM ((renderShot dc)) shots 
  mapM ((renderDrop dc)) drops
  set dc [textColor := white]
  drawText dc (show score) (Point 650 50) []
  return ()

-- | ???
renderCircle :: DC a -> Geometry.Circle -> IO ()
renderCircle dc c = do
  Graphics.UI.WX.circle dc (point (round $ c^.x) (round $ c^.y)) (round $ c^.r) []

-- | draw background image
renderBackground :: DC a -> (Int,Int) -> IO ()
renderBackground dc pos = do 
  drawBitmap dc bgImage (Point 0 $ fst pos) True []
  drawBitmap dc bgImage (Point 0 $ fst pos - round Geometry.height*2) True []
  return ()

-- | 
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

intersectionShotDrop :: Shots -> Drops -> Bool
intersectionShotDrop shots drops = any (\ shot -> any (\ drop -> intersects (drop ^. Geometry.circle) (shot ^. Geometry.circle)) drops) shots

