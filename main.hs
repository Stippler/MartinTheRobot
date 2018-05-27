
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}

import Graphics.UI.WX hiding (Event)
import Reactive.Banana 
import Reactive.Banana.WX hiding (compile)
import Geometry

width = 800
height = 600

main :: IO ()
main = start $ do 
  f <- frame [text:="Martin der Roboter"]
  p <- panel f [ ]
  -- set f [ layout := minsize (sz width height) $ widget p ]
  t <- timer f [ interval := 10 ]
  t2 <- timer f [ interval := 100 ]
  
  let networkDescription :: MomentIO ()
      networkDescription = mdo
      
        etick <- event0 t command
        etick2 <- event0 t2 command
        ekey <- event1 p keyboard 
        emouse <- event1 p mouse -- mouse events

        let eleft  = filterE ((== KeyLeft ) . keyKey) ekey
            eright = filterE ((== KeyRight) . keyKey) ekey
            eup    = filterE ((== KeyUp   ) . keyKey) ekey
            edown  = filterE ((== KeyDown ) . keyKey) ekey
                
        (bPlayerPosition :: Behavior (Circle))
            <- stepper (Circle 0 0 0) $
                 toCircle 15 <$> (filterJust $ justMove <$> emouse)
        
        (bShot :: Behavior [Circle])
                <- accumB [] $ unions
                    [ addShot <$> (((\a b->if a then (Just b) else Nothing) <$> bShooting <*> (fmap (setRadius 5) bPlayerPosition)) <@ etick2 )
                    , moveShot <$ etick
                    ]
        
        (bRainDrops :: Behavior [Circle])
                <- accumB [Circle 20 10 10, Circle 50 10 10] $ unions
                    [ collisionWithShots <$>  bShot <@ (fallingDrops <$ etick)
                    --, (collisionWithShots <$> bShot <@ etick) 
                    ]
        
        
        
        (bShooting :: Behavior Bool)
            <- stepper False $ (filterJust $ justPressed <$> emouse)
              
              
                   
        
        
        
        
        
        
        bpaint <- stepper (\_dc _ -> return ()) $ (render <$> bPlayerPosition <*> bShot <*> bRainDrops ) <@ etick
      
        sink  p [on paint :== bpaint]
        reactimate $ repaint p <$ etick
        
        return ()
  
  network <- compile networkDescription
  actuate network

  return ()

render :: Circle -> [Circle] -> [Circle] -> DC a -> Rect -> IO ()
render circle shots circles dc viewArea = do
  set dc [brushColor := blue, brushKind := BrushSolid]
  renderCircle dc circle
  set dc [brushColor := red, brushKind := BrushSolid]
  mapM (renderCircle dc) circles
  set dc [brushColor := green, brushKind := BrushSolid]
  mapM (renderCircle dc) shots
  return ()

renderCircle :: DC a -> Circle -> IO ()
renderCircle dc circle = do
  Graphics.UI.WX.circle dc (point (getX circle) (getY circle)) (getRadius circle) []

collisionWithShots :: [Circle] -> [Circle] -> [Circle]
collisionWithShots [] [] = []
collisionWithShots [] drops = drops
collisionWithShots shots drops = filter (not . (intersectsList shots)) drops

fallingDrops :: [Circle] -> [Circle]
fallingDrops circles = (moveY 1) <$> circles

moveShot :: [Circle] -> [Circle]
moveShot circles = filter ((>(-20)).(getY)) $ (moveY (-1)) <$> circles

addShot :: Maybe Circle -> [Circle] -> [Circle]
addShot Nothing circles = circles
addShot (Just circle) circles = circle:circles

justPressed :: EventMouse -> Maybe Bool
justPressed (MouseLeftDown _ _) = Just True
justPressed (MouseLeftUp _ _) = Just False
justPressed _ = Nothing

justMove :: EventMouse -> Maybe Point
justMove (MouseMotion pt _) = Just pt
justMove (MouseLeftDrag pt _) = Just pt
justMove (MouseRightDrag pt _) = Just pt
justMove _                  = Nothing

