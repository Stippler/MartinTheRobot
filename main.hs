
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}

import Graphics.UI.WX hiding (Event)
import Reactive.Banana 
import Reactive.Banana.WX hiding (compile)

width = 400

main :: IO ()
main = start $ do 
  f <- frame [text:="Martin der Roboter"]
  p <- panel f []
  t <- timer f [ interval := 10 ]
  
  let networkDescription :: MomentIO ()
      networkDescription = mdo
      
        etick <- event0 t command
        ekey <- event1 p keyboard

        let eleft  = filterE ((== KeyLeft ) . keyKey) ekey
            eright = filterE ((== KeyRight) . keyKey) ekey
            eup    = filterE ((== KeyUp   ) . keyKey) ekey
            edown  = filterE ((== KeyDown ) . keyKey) ekey
          
        (brect :: Behavior (Point2 Int))
            <- accumB (Point 20 30) $ unions
              [ goLeft  <$ eleft
              , goRight <$ eright
              , goUp    <$ eup
              , goDown  <$ edown
              ]
        let
          goLeft  (Point x y) = Point (x-5)  y
          goRight (Point x y) = Point (x+5)  y
          goUp    (Point x y) = Point  x    (y-5)
          goDown  (Point x y) = Point  x    (y+5)
      
        bpaint <- stepper (\_dc _ -> return ()) $ (renderRect <$> brect) <@ etick
      
        sink  p [on paint :== bpaint]
        reactimate $ repaint p <$ etick
        
        return ()
  
  network <- compile networkDescription
  actuate network

  return ()
  


renderRect :: Point -> DC a -> Rect -> IO ()
renderRect point dc viewArea = do
  set dc [brushColor := black, brushKind := BrushSolid]
  drawRect dc (Rect (pointX point) (pointY point) 50 50) []
