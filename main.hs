
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
  
  let networkDescription :: MomentIO ()
      networkDescription = mdo
      
        etick <- event0 t command
        ekey <- event1 p keyboard 
        emouse <- event1 p mouse -- mouse events

        let eleft  = filterE ((== KeyLeft ) . keyKey) ekey
            eright = filterE ((== KeyRight) . keyKey) ekey
            eup    = filterE ((== KeyUp   ) . keyKey) ekey
            edown  = filterE ((== KeyDown ) . keyKey) ekey
          
        (bPlayerPosition :: Behavior (Circle)) <-
            stepper (Circle 0 0 0) $
                toCircle 15 <$> (filterJust $ justMove <$> emouse)
        
        (bRainDrops :: Behavior [Circle])
                <- accumB [Circle 10 10 10] $ unions
                    [fallingDrops <$ etick]
        
        bpaint <- stepper (\_dc _ -> return ()) $ (render <$> bPlayerPosition <*> bRainDrops) <@ etick
      
        sink  p [on paint :== bpaint]
        reactimate $ repaint p <$ etick
        
        return ()
  
  network <- compile networkDescription
  actuate network

  return ()
  


render :: Circle -> [Circle] -> DC a -> Rect -> IO ()
render circle circles dc viewArea = do
  set dc [brushColor := black, brushKind := BrushSolid]
  renderCircle dc circle
  head $ map (renderCircle dc) circles
  return ()


renderCircle :: DC a -> Circle -> IO ()
renderCircle dc circle = do
  Graphics.UI.WX.circle dc (point (getX circle) (getY circle)) (getRadius circle) []

fallingDrops :: [Circle] -> [Circle]
fallingDrops circles = (moveY 1) <$> circles


justMove :: EventMouse -> Maybe Point
justMove (MouseMotion pt _) = Just pt
justMove (MouseLeftDrag pt _) = Just pt
justMove (MouseRightDrag pt _) = Just pt
justMove _                  = Nothing

